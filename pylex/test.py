# Author: Tim Knutson - u0851247 - tkkemo@gmail.com
from socket import*
import socket
import sys
import time
from multiprocessing import Process

#Class represents a simple server proxy that can handle GET method
class ProxyServer:

    #initialize server to specified port number
    def __init__(self, port):
	self.serverPort = port
	self.serverAddress = ''

    #attect to connect to socket and if so call listen
    def start(self):
	#if socket is busy, retry 5 times
	count = 5
	while count > 0:
	    try:
		self.serverSocket = socket.socket(AF_INET,SOCK_STREAM)
		self.serverSocket.bind((self.serverAddress, self.serverPort))
		self.serverSocket.listen(0) # create listening socket
		print 'The server is ready to receive.'
		self.listen()
	    except socket.error:
		print 'Socket unavailable, trying again <attempt ' + str(-1*(count-6)) + '/5>...'
		count -= 1
		time.sleep(12)
		pass
	    else:
		print 'Socket error, try again later.'
		break

    #method represents the main loop that listens for incoming clients
    def listen(self):
	while 1:
	    webClientSocket, addr = self.serverSocket.accept()

	    p = Process(target = self.handle, args=(webClientSocket,))
	    p.start()
	    webClientSocket.close()


    #called in order to handle a client request to server
    def handle(self, clientSocket):
	chunk = clientSocket.recv(1024)
	clientRequest = chunk
	while not ('\r\n\r\n' in clientRequest) and chunk != '':
	    chunk = clientSocket.recv(1024)
	    clientRequest += chunk
	clientRequest = clientRequest.strip()

	#TODO robust error handling
	#TODO error message format

	#if message is empty return
	if clientRequest == '':
	    return

	lines = clientRequest.splitlines()
	requestLine = lines.pop(0).split()

	#if method is not get send error
	if requestLine[0] != "GET":
	    clientSocket.send("HTTP/1.0 501 Method Not Implemented\r\n");
	    return

	#make sure formatted correctly
	if len(requestLine) < 3:
	    clientSocket.send("HTTP/1.0 Bad Request\r\n");
	    return

	#check to make sure hostname is specified somewhere
	if not requestLine[1].split('/',1)[0] and (len(lines) < 1 or lines[0].split()[0] != 'Host:'):
	    clientSocket.send("HTTP/1.0 Bad Request\r\n");
	    return

	webServerAddress, webServerPath, webServerPort = self.parse(requestLine, lines, clientSocket)

	#TODO check cache
	#TODO see if cache needs updating

	#find connection header line and ditch it, we'll add it back later with 'close' value
	for line in lines:
	    splitLine = line.split()
	    if splitLine[0] == 'Connection:':
		lines.remove(line)

	HTTPRequest = self.constructRequest(webServerAddress, webServerPath, lines)

	self.cache(clientSocket, webServerAddress, webServerPath, webServerPort, HTTPRequest)

    
    #parse for address, path, and port
    def parse(self, requestLine, lines, clientSocket):
	address = ''
	path = '/'
	port = 80

	#check for hostname line
	#fill path and address
	if len(lines) > 0 and lines[0].split()[0] == 'Host:':
	    address = lines.pop(0).split()[1]
	    if 'http:' in requestLine[1]:
		if len(requestLine[1].split('/',3)) > 3:
		    path += requestLine[1].split('/',3)[3]
	    else:
		if len(requestLine[1].split('/')) > 1:
		    path += requestLine[1].split('/',1)[1]
	else:
	    URL = requestLine[1]
	    address = URL.split('/',1)[0]
	    if address == "http:":
		urlTokens = URL.split('/',3)
		address = urlTokens[2]
		if len(urlTokens) > 3:
		    path += urlTokens[3]
	    elif '/' in  URL:
		if len(URL.split('/',1)[1]) > 1:
		    path += URL.split('/',1)[1]

	#check for specified port number
	if len(address.split(':')) > 1:
	    port = address.split(':')[1]
	    address = address.split(':')[0]
	elif "http:" in requestLine[1]:
	    sub_url = requestLine[1].split('/')[2]
	    if ':' in sub_url:
		port = sub_url.split(':')[1]
	else:
	    sub_url = requestLine[1].split('/')[0]
	    if ':' in sub_url:
		port = sub_url.split(':')[1]

	return (address, path, port)

    def cache(self, clientSocket, address, path, port, request):
	try:
	    print address+path

	    filename = str(hash(address + path))
	    f = open("cache/" + filename, 'r')
	    print "we have the page"
	    webServerResponse = f.read()
	    clientSocket.send(webServerResponse)
	except IOError:
	    print "asked the server"
	    webServerResponse = self.forwardRequest(request, address, port)
            if address != "www.google-analytics.com" and address != "l.ooyala.com":
		filename = str(hash(address + path))
		f = open("cache/"+filename, 'w')
		f.write(webServerResponse)
	    clientSocket.send(webServerResponse)


    #constructs a proper response with 'Host: hostname, relative path, and 'Connection: close' included
    def constructRequest(self, address, path, lines):
	request = 'GET ' + str(path) + ' HTTP/1.0\r\n'
	request += 'Host: ' + str(address) + '\r\n'
	for line in lines:
	    request += line + '\r\n'
	request += 'Connection: close\r\n\r\n'
	return request

    #opens connection with web server and returns web server response
    def forwardRequest(self, request, address, port):
	webServerSocket = socket.socket(AF_INET, SOCK_STREAM)
	webServerSocket.connect((address,int(port)))
	webServerSocket.send(request)
	chunk = webServerSocket.recv(1024)

	while not ('\r\n\r\n' in chunk):
	    chunk += webServerSocket.recv(1024)

	webServerResponse = chunk

	content_length = 0
	responseHeader = webServerResponse.split('\r\n\r\n',1)[0]
	for line in responseHeader.splitlines():
	    if line.split()[0] == 'Content-Length:':
		content_length = int(line.split()[1])

	bytesofar = len(chunk.split('\r\n\r\n',1)[1])
	if content_length:
	    while bytesofar < content_length and chunk != '':
		chunk = webServerSocket.recv(1024)
		webServerResponse += chunk
		bytesofar += len(chunk)
	else:
	    while chunk != '':
		chunk = webServerSocket.recv(1024)
		webServerResponse += chunk

	webServerSocket.close()
	
	return webServerResponse


def main():
    if len(sys.argv) < 2:
	print"Please specify a port number."
	exit()
    elif len(sys.argv) > 2:
	print"Invalid arguments"
    proxy = ProxyServer(int(sys.argv[int(1)]))
    proxy.start()

if __name__ == "__main__":
    main()
# Author: Tim Knutson - u0851247 - tkkemo@gmail.com

var http = require("http");
var server = http.createServer(function(request, response) {
  response.writeHead(200, {"Content-Type": "text/html"});
  response.write("<!DOCTYPE html>");
  response.write("<html>");
  response.write("<head>");
  response.write("<title>Hello dockercook</title>");
  response.write("</head>");
  response.write("<body>");
  response.write("Hello dockercook!");
  response.write("</body>");
  response.write("</html>");
  response.end();
});

console.log("I've been cooked! ouch!");
server.listen(80);

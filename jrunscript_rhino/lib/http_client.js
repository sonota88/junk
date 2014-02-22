function withInputStreamReader(inputStream, encoding, proc){
  var isr = new InputStreamReader(
    inputStream,
    encoding);
  try{
    proc(isr);
  }finally{
    isr.close();
  }
}

function withBufferedReader(inputStreamReader, proc){
  var br = new BufferedReader(inputStreamReader);
  try{
    proc(br);
  }finally{
    br.close();
  }
}

function readAllFromInputStream(is, encoding, proc){
  var sb = new java.lang.StringBuilder();

  withInputStreamReader(is, encoding, function(isr){
    withBufferedReader(isr, function(br){
      var line;
      while ((line = br.readLine()) != null) {
        sb.append(line).append("\n");
      }
    });
  });

  return sb.toString();
}

function makeParamString(params){
  var pairs = [];
  for(var k in params){
    pairs.push(k + "=" + encodeURIComponent(params[k]));
  }
  return pairs.join("&");
}

function post(urlStr, params){
  
  // java.net.URL
  var url = new URL(urlStr);

  // java.net.HttpURLConnection
  var connection = null;
  var status, body;
  
  try {
    connection = url.openConnection();
    connection.setDoOutput(true);
    connection.setRequestMethod("POST");

    var writer = new BufferedWriter(
      new OutputStreamWriter(connection.getOutputStream(), "UTF-8"));
    writer.write(makeParamString(params));
    writer.flush();

    status = connection.getResponseCode();

    if (status == HttpURLConnection.HTTP_OK) {
      body = readAllFromInputStream(connection.getInputStream(), "UTF-8");
    }
  } finally {
    if (connection != null) {
      connection.disconnect();
    }
  }
  
  return {
    status: status
    , body: body
  };
}

exports.httpClient = {
  post: post
};

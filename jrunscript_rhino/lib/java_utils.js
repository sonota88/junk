function toJavaString(javaScriptString){
  return new java.lang.String(javaScriptString);
};

function createArray(type, size){
  var _type;
  if(type === "Byte"){
    _type = java.lang.Byte.TYPE;
  }else{
    _type = type;
  }
  return java.lang.reflect.Array.newInstance(_type, size);
};

exports.JavaUtils = {
  toJavaString: toJavaString
  , createArray: createArray
};

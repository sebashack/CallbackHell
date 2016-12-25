const enc = { encoding: 'utf-8' };

/** 
   Note that onSuccess and onFailure are functions which receive an
   argument of type string and return another function which receives
   no arguments and performs the effects.
**/

exports.readFileImpl = function(path, onSuccess, onFailure) {
  return function() {
    require('fs').readFile(path, enc, function(error, data) {
      if (error) {
	onFailure(error.code)();
      } else {
	onSuccess(data)();
      }
    });
  };
};

exports.writeFileImpl = function(path, data, onSuccess, onFailure) {
  return function() {
    require('fs').writeFile(path, data, enc, function(error) {
      if (error) {
	onFailure(error.code)();
      } else {
	onSuccess();
      }
    });
  };
};



// ||||||| SETIMEOUT RUNTIME REPRESENTATION (SOLVED)

exports.setTimeOutImpl = function(mlsecs, cb) {
  return function() {
    setTimeout(cb, mlsecs);
  };
}; 
/* ||||||||||||||||||||||||||||||||||||| */

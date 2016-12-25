request = require('request');

exports.getImpl = function(uri, done, fail) {
  return function() {
    request(uri, function(err, _, body) {
      if(err) {
	fail(err)();
      } else {
	done(body)();
      }
    });
  };
};



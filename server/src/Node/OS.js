var os = require('os')
exports.numCpus = function() { return os.cpus().length }

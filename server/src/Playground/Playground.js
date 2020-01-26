var fs = require("fs-extra")

exports.copyImpl = function(src, target) {
        return fs.copy(src, target, { overwrite: true, dereference: true })
}
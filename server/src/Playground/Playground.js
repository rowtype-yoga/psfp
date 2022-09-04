import {copy} from "fs-extra"

export const copyImpl = function(src, target) {
        return copy(src, target, { overwrite: true, dereference: true })
}

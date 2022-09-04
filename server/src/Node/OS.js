import { cpus } from 'os'
export const numCpus = function() { return cpus().length }

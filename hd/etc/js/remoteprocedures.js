import { RpcClient } from "./rpc.js";

/** @param {Object} obj
 *  @returns {boolean}
 */
function isStringArray(obj) {
  if (Array.isArray(obj)) {
    return obj.every(el => typeof el === 'string');
  } else {
    return false;
  }
}

export class RemoteProcedures extends RpcClient {
  async ping() {
    return this.call("ping", [...arguments]);
  }

  /** Fuzzying search in an index.
   *  @param {string} index - Name of the index.
   *  @param {string} pattern - Pattern.
   *  @returns {Promise<Array<string>>} */
  async searchIndex(index, pattern) {
    const obj = await this.call("search_index", [...arguments]);
    // if (isStringArray(obj)) {
      return obj;
    // } else {
    //   throw new Error("invalid json schema");
    // }
  }
}

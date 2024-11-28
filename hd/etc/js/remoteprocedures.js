import { RpcClient } from "./rpc.js";

export class RemoteProcedures extends RpcClient {
  async ping() {
    return this.call("ping", [...arguments]);
  }

  /** Fuzzying search in an index.
   *  @param {string} index - Name of the index.
   *  @param {string} pattern - Pattern. */
  async searchIndex(index, pattern) {
    return this.call("search_index", [...arguments]);
  }
}

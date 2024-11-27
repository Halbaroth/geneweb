import { RpcClient } from "./rpc.js";

export class RemoteProcedures extends RpcClient {
  async ping() {
    return this.call("ping", [...arguments]);
  }
}

import { Task, TaskQueue } from "./task.js";

export class WebSocketHandler {
  constructor(addr, open_handler, message_handler,
    max_retries = 5, retry_delay = 1000) {
    // We properly close the websocket when we leave the page.
    window.addEventListener("beforeunload", (_event) => {
      this.close();
    });

    this.addr = addr;
    this.queue = new TaskQueue();
    this.retries = 0;
    this.max_retries = max_retries;
    this.retry_delay = retry_delay;
    this.terminate = false;

    this.ws = new WebSocket(this.addr);

    this.ws.onopen = (event) => {
      console.log(`connection opened on ${this.addr}`);
      open_handler (this, event);
    };

    this.ws.onmessage = (event) => {
      this.queue.run(() => {
        const obj = JSON.parse(event.data);
        message_handler (obj);
      });
    };

    this.ws.onclose = async (event) => {
      console.log(`connection closed with code = ${event.code},
        reason = ${event.reason}`);
      //
      // if (!this.terminate && this.retries < this.max_retries) {
      //   this.retries++;
      //   console.log("attempting to reconnect...");
      //   await this.reconnect();
      // } else {
      //   console.log("reached the maximum retries");
      // }
    };

    this.ws.onerror = (event) => {
      console.log(`connection on ${this.addr} closed due to error`);
    };
  }

  // async reconnect() {
  //   if (this.ws === null ||
  //       this.ws.readyState !== WebSocket.OPENING ||
  //       this.ws.readyState !== WebSocket.OPEN) {
  //     return;
  //   }
  //
  //   const delay = this.retry_delay * Math.pow(2, this.retries);
  //   await new Promise(resolve => setTimeout(resolve, delay));
  //   if (!this.terminate) {
  //     this.connect();
  //   }
  // }

  async send(obj) {
    // if (this.ws === null || this.ws.readyState !== WebSocket.OPEN) {
    //   await this.reconnect();
    // }
    //
    await this.queue.run(() => {
      let json = JSON.stringify(obj);
      this.ws.send(json);
    });
  }

  close() {
    this.queue.cancel_all();
    this.ws.close();
  }
}

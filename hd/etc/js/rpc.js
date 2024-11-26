/** Response schema
 *  @typedef {Object} ResponseSchema
 *  @property {number} id
 *  @property {string} error
 *  @property {Object} payload */

/** Valide a json message against the above schema.
 *  @param {ResponseSchema} json
 *  @returns {boolean} */
function validateJSONResponse(json) {
  if (typeof json !== 'object' || json === null) {
    return false;
  }

  if (!json.id || typeof json.id !== 'number') {
    return false;
  }

  if (json.error && typeof json.error !== 'string') {
    return false;
  }

  if ((!json.error || typeof json.error !== 'string') && !json.payload) {
    return false;
  }

  return true;
}

/** Parse and validate JSON response.
 *  @param {string} str
 *  @returns {ResponseSchema | null}
 */
function parseResponse(str) {
  try {
    const parsed = JSON.parse(str);
    if (!validateJSONResponse(parsed)) {
      console.error(`invalidate JSON message, got: ${str}`);
      return null;
    } else {
      return parsed;
    }
  } catch (error) {
    console.error(`cannot parse JSON message: ${error}`);
    return null;
  }
}

/** @typedef {(event: MessageEvent<string>) => void} MessageCallback */

/** Create a promise on a WebSocket. The promise is resolved as soon
 *  as the connection is established.
 *  @param {string} url
 *  @param {MessageCallback} onMessage
 *  @returns {Promise<WebSocket>}
 */
const websocketPromise = (url, onMessage) => {
  return new Promise((resolve, reject) => {
    const socket = new WebSocket(url);
    /** @param {Event} _ev */
    const onOpen = (_ev) => {
      resolve(socket);
    };
    socket.addEventListener("message", onMessage);
    socket.addEventListener("open", onOpen, { once: true });
    socket.addEventListener("error", reject, { once: true });
  });
};

/** Produce a random number in the range [min, max].
 *  @param {number} min
 *  @param {number} max
 *  @returns {number} */
function random_integer(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

export class RpcClient {
  /** @type Promise<WebSocket> */
  socket;

  /**
   * @typedef {(error: string, payload: object) => void} Callback
   * @type Map<number, Callback> */
  waiting;

  /** Open a Websocket on the url.
   *  @param {string} url - the host.
   */
  constructor(url) {
    this.waiting = new Map();
    this.socket = websocketPromise(url, this.#onMessage.bind(this));
  }

  /** Call a remote procedure.
   *  @param {string} procedure - the remote procedure name.
   *  @param {...any} args - the list of its arguments.
   *  @return {Promise<object>} */
  call(procedure, args) {
    const id = random_integer(0, 10000000);
    return new Promise((resolve, reject) => {
      /** @param {string} error
       *  @param {object} payload */
      const callback = (error, payload) => {
        if (error !== null) {
          reject(`server error: ${error}`);
        } else {
          resolve(payload);
        }
      };

      this.socket.then((result) => {
        this.waiting.set(id, callback);
        result.send(
          JSON.stringify({
            id,
            procedure,
            args: [...args]
          })
        )
      });
    });
  }

  /** Execute the callback corresponding to the identifier in the message.
   *  @param {MessageEvent<string>} event
   */
  #onMessage(event) {
    let response = parseResponse(event.data);
    if (response !== null) {
      let callback = this.waiting.get(response.id);
      if (callback === undefined) {
        console.error("BUG");
      } else {
        this.waiting.delete(response.id);
        callback(response.error, response.payload);
      }
    }
  }
}

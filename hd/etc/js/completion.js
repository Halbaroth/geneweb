import { RemoteProcedures } from "./remoteprocedures.js";

/** Counts the number of UTF-16 characters.
 *  @param {string} str
 *  @returns {number}
 */
function stringLength(str) {
  return [...str].length;
}

/** Add the completion feature for an input field.
 *  @param {RemoteProcedures} rp
 *  @param {HTMLInputElement} input
 *  @param {HTMLDataListElement} datalist
 */
export function addCompletion(rp, input, datalist) {
  input.addEventListener('input', async (_event) => {
    if (stringLength(input.value) >= 3) {
      try {
        const values = await rp.searchIndex("communes", input.value);
        datalist.innerHTML = '';
        console.log(values);
        values.forEach(item => {
          const option = document.createElement("option");
          option.value = item;
          datalist.appendChild(option);
        });
      } catch (error) {
        console.error(error);
      }
    } else {
      datalist.innerHTML = '';
    }
  });
}

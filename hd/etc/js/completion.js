import { RemoteProcedures } from "./remoteprocedures.js";

/** Counts the number of UTF-16 characters.
 * @param {string} str
 * @returns {number}
 */
function stringLength(str) {
  return [...str].length;
}
/**
 * @param {string} content
 * @param {(ev : MouseEvent) => any} callback
 * @return {HTMLDivElement}
 */
function createOption(content, callback) {
  const div = document.createElement("div");
  div.className = 'dropdown-item';
  div.textContent = content;
  div.addEventListener('click', callback);
  return div;
}

/** Add the completion feature for an input field.
 *  @param {RemoteProcedures} rp
 *  @param {HTMLInputElement} input
 *  @param {HTMLDivElement} list
 */
export function addCompletion(rp, input, list) {
  // input.addEventListener('click', (_event) => {
  //   list.style.display =
  //     list.style.display === 'block' ? 'none' : 'block';
  // });

  document.addEventListener('click', (event) => {
    if (event.target instanceof Node) {
      if (!list.contains(event.target)) {
        list.style.display = 'none';
      }
    }
  });

  input.addEventListener('input', async (_event) => {
    if (stringLength(input.value) >= 3) {
      list.style.display = 'block';

      try {
        const values = await rp.searchIndex("communes", input.value);
        list.innerHTML = '';
        console.log(values);
        values.forEach(item => {
          const div = createOption(item, (_ev) => {
            input.value = item;
            list.style.display = 'none';
          });
          list.appendChild(div);
        });
      } catch (error) {
        console.error(error);
      }
    } else {
      list.innerHTML = '';
    }
  });
}

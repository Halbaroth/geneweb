import { RemoteProcedures } from "./remoteprocedures.js";

/** Counts the number of UTF-16 characters.
 * @param {string} str
 * @returns {number}
 */
function stringLength(str) {
  return [...str].length;
}

export class DropDown {
  /** @type HTMLInputElement */
  input;

  /** @type HTMLDivElement */
  list;

  /** @type Number | null */
  selectedIndex;

  /** Set up the logic for a dropdown list using the input field input
   *  and the div list to store the choices.
   * @param {HTMLInputElement} input
   * @param {HTMLDivElement} list
   */
  constructor(input, list) {
    this.selectedIndex = null;
    this.input = input;
    this.list = list;

    document.addEventListener('click', (event) => {
      if (event.target instanceof Node) {
        if (!list.contains(event.target)) {
          this.hide();
        }
      }
    });

    input.addEventListener('keydown', this.#onKeyDown.bind(this));

    list.addEventListener('mouseover', (_event) => {
      this.selectedIndex = null;
      for (let child of this.list.children) {
        child.classList.remove('dropdown-content-hovered');
      }
    });
  }

  show() {
    this.list.style.display = 'block';
  }

  hide() {
    this.list.style.display = 'none';
  }

  // FIXME: this fonction won't behave for dropdown list of
  // size < 2.
  selectPrevious() {
    if (this.selectedIndex !== null) {
      const prev = this.list.children[this.selectedIndex];
      prev.classList.remove('dropdown-content-hovered');
    } else {
      this.selectedIndex = 0;
    }

    if (this.selectedIndex > 0) {
      this.selectedIndex--;
    } else {
      this.selectedIndex = this.list.children.length - 1;
    }

    const curr = this.list.children[this.selectedIndex];
    curr.classList.add('dropdown-content-hovered');

    this.input.value =
      (curr.textContent === null) ? '' : curr.textContent;
  }

  // FIXME: this fonction won't behave for dropdown list of
  // size < 2.
  selectNext() {
    if (this.selectedIndex !== null) {
      const prev = this.list.children[this.selectedIndex];
      prev.classList.remove('dropdown-content-hovered');
    } else {
      this.selectedIndex = this.list.children.length - 1;
    }

    if (this.selectedIndex < this.list.children.length - 1) {
      this.selectedIndex++;
    } else {
      this.selectedIndex = 0;
    }

    const curr = this.list.children[this.selectedIndex];
    curr.classList.add('dropdown-content-hovered');

    this.input.value =
      (curr.textContent === null) ? '' : curr.textContent;
  }

  /**
   * @param {KeyboardEvent} event
   */
  #onKeyDown(event) {
    this.show();

    switch (event.key) {
      case 'ArrowDown':
        this.selectNext();
        break;

      case 'ArrowUp':
        this.selectPrevious();
        break;
    }
  }

  flush() {
    this.list.innerHTML = '';
    this.selectedIndex = null;
  }

  /** Replace all the values of the dropdown list.
   * @param {Array<string>} values
   */
  replace(values) {
    this.flush();
    values.forEach(item => {
      const div = document.createElement("div");
      div.className = 'dropdown-item';
      div.textContent = item;
      div.addEventListener('click', (_ev) => {
        this.input.value = item;
        this.hide();
      });
      this.list.appendChild(div);
    });

  }
}

/** Setup the dropdown list logic with the completion feature.
 * @param {HTMLInputElement} input
 * @param {HTMLDivElement} list
 * @param {RemoteProcedures} rp
 */
export function dropDownWithCompletion(input, list, rp) {
  const lst = new DropDown(input, list);

  lst.input.addEventListener('input', async (_event) => {
    if (stringLength(lst.input.value) >= 3) {
      lst.show();

      try {
        const values = await rp.searchIndex("roglo-v7", lst.input.value);
        lst.replace(values);
      } catch (error) {
        console.error(error);
      }
    } else {
      lst.flush();
    }
  });

  return lst;
}

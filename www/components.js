// preact is a library for javascript frontend
import { render, createRef, createContext } from 'preact';
import { useSignal, useComputed, useSignalEffect } from "@preact/signals";
import { html } from 'htm/preact';
import register from 'preact-custom-element';

// this is implemented by me
import { onMouseDownHandler, onTouchStartHandler } from './utils.js';

// this is an preact component. It is basically an advanced html element.
// It gets the html properties as input and returns a DOM like object.
// It gets reevaluated every time props change.
export function knob(props) {
  // createRef allows us to access html elements, we will use that later.
  const svg = createRef();
  const elem = createRef();
  // beforeunload gets called before we close the window.
  addEventListener("beforeunload", (event) => {
    $(elem.current.parentNode).trigger("data", {close: true});
  });

  // these are our properties: value, min and max
    // useSignal() returns a singleton so we get the same signal
    // everytime.
  const val = useSignal();
  val.value = parseFloat(props.value);
  const min = useSignal();
  min.value = parseFloat(props.min || 0);
  const max = useSignal();
  max.value = parseFloat(props.max || 100);
  // this is similar to watch. Again in a singleton version. This function gets called every time some (relevant) signal changes.
  useSignalEffect(() => {
    let value = val.value;
    elem.current ? $(elem.current.parentNode).trigger("data", {value: value}) : ""
  });

  // these are also singletons. These are derived values that get updated everytime a relevant signal changes.
  const range = useComputed(() => max.value - min.value)
  const percentage = useComputed(() => (val.value-min.value) / range.value)
  const step = useSignal(parseFloat(props.step || 1));
  const sensitivity = useSignal(parseFloat(props.sensitivity || 200));
  const x = useComputed(() => 8*Math.cos(percentage.value*Math.PI*2-(Math.PI/2))+8)
  const y = useComputed(() => 8*Math.sin(percentage.value*Math.PI*2-(Math.PI/2))+8)
  const d1 = useComputed(() => percentage.value <= 0.5 ? `M 8 0 A 8 8 0 0 1 ${x} ${y}` : `M 8 0 A 8 8 0 0 1 8 16`)
  const d2 = useComputed(() => percentage.value > 0.5 ? `M 8 16 A 8 8 0 0 1 ${x} ${y}` : `M 8 16 A 8 8 0 0 1 8 16`)
  // this is a weird construct I usually use it to avoid writing my mousemove eventhandlers over and over again.
  // it is basically a getter and a setter that gets called whenever the mouse moves (after it got registered see below).
  const mm = {
    y: 0,
    _x: 0,
    get x() {
      this._x = val.value;
      return 0;
    },
    set x(value) {
      let n = Math.round((value*range.value/sensitivity.value)/step.value)*step.value
      val.value = Math.max(min.value, Math.min(max.value,this._x+n));
      return true;
    }
  }


  // html parses the string and turns it into a DOM like object. `Backtick strings and ${jscode} are JavaScripts template strings`.
  // More info: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals
  // and: https://github.com/developit/htm for the used tag function
  return html`
    <div class="knob" ref=${elem}>
      <svg ref=${svg} xmlns="http://www.w3.org/2000/svg" onMouseDown=${onMouseDownHandler(mm)} onTouchStart=${onTouchStartHandler(mm)} viewBox="0 0 16 16">
        <path d="${d1}" stroke="lightgreen" fill="none" stroke-width="2" />
        <line x1="50%" y1="50%" x2=${x} y2=${y} id="pointer" />
      </svg>
      <span class="value">${val}</span>
    </div>
  `
}

// this turns our component function into a custom element so now we can use <o-knob></o-knob> in html.
// Whenever value changes the function gets reevaluated and the dom might change.
// shadow is an option. Shadow DOM is a new feature that we do not want to use.
register(knob, 'o-knob', ['value'], { shadow: false });

//
// multislider.js
//
// definition of multislider mouse and event handling in the client.
// For this to work, slider.js needs to have been
// loaded. multislider() gets called with the container of the sliders
// already containing the sliders as uninitialized div elements with
// their idx set in the data-idx attribute.
//
// **********************************************************************
// Copyright (c) 2023 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
//
// Revision history: See git repository.
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the Gnu Public License, version 2 or
// later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
// of this agreement.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// **********************************************************************

function mslider(elem, idx, numSliders) {
//    var barColor      = config.barColor || 'transparent';
//    var thumbColor    = config.thumbColor || 'black';
    var thumb         = 'false';

    var slider;
    var idx = idx;
    if (elem.nodeType == undefined)
        slider = elem.get(0);
    else
        slider = elem;

    function disableDrag (elem) {
       // elem.ondragstart = undefined
    }

    var sliderBar = document.createElement("div");
    sliderBar.setAttribute('class', 'sliderbar');
    slider.appendChild(sliderBar);

    disableDrag(slider);
    disableDrag(sliderBar);

    const valChangeEvent = new Event("valuechange");
    const pxRegex = /([0-9]+).*/
    var offsetTop = slider.offsetTop;
    var offsetLeft = slider.offsetLeft;
    var sliderHeight;
    var sliderWidth;
    var thumbWidth;
    var colors;
    var minValue;
    var maxValue;
    var value;
    var mapping;
    var direction;
    var valFunction;
    var valFunctionRev;
    var valueRange;
    var valueRatio;
    var valueLogRatio;
    var calcBarSize;
    var mouseMoveListener;
    var oldValue = -1;
    var fraction = -1;
    var oldFraction = -2;

    var style = window.getComputedStyle(slider, null);
    var clipZero = slider.getAttribute('clip-zero');
    var thumbColor = slider.getAttribute('thumb-color');
    var barColor = slider.style.getPropertyValue('--bar-color');
    
    sliderBar.style.background = barColor;
//    sliderBar.style.width = '12.667%';

    var getFraction;

    function clamp(number, min, max) {
        return Math.max(min, Math.min(number, max));
    }

    // Utils
    
    function getValFraction (val) {
        return ((val - minValue) / valueRange);
    }

    function getYFraction (event) {
        let rect = slider.getBoundingClientRect();
        let localYFrac = (rect.height + rect.top - event.clientY) / (rect.height - (3 * thumbWidth));
//        console.log(rect.height + ', ' + rect.top + ', ' + event.clientY + ', ' + thumbWidth + ', ' + localYFrac);
        return clamp(localYFrac, 0, 1);
    }

    function getYFractionRev (event) {
        return (1 - getYFraction(event));
    }

    function getXFraction (event) {
        let sliderRect =  slider.getBoundingClientRect();
        let localXFrac = ((event.clientX - sliderRect.left)) / (sliderRect.width - (3 * thumbWidth));
        return clamp(localXFrac, 0, 1);
    }

    function getXFractionRev (event) {
        return (1 - getXFraction(event));
    }

    function calcBarHeight (YFraction) {
        let sliderRect =  slider.getBoundingClientRect();
        let newBarSize = (YFraction * (sliderRect.height - (3 * thumbWidth))) + 'px';
        if (newBarSize != oldBarSize) {
            oldBarSize = newBarSize;
            sliderBar.style.height = newBarSize;
        }
    }

    function calcBarWidth (XFraction) {
        let sliderRect =  slider.getBoundingClientRect();
        let newBarSize = (XFraction * (sliderRect.width - (3 * thumbWidth))) + 'px';
        if (newBarSize != oldBarSize) {
            oldBarSize = newBarSize;
            sliderBar.style.width = newBarSize;
        }
    }

    function linVal (frac) { // frac -> val
        return (minValue + (frac * valueRange));
    }

    function linValRev (val) { // val -> frac
        return ((val - minValue)/ valueRange);
    }

    function logVal (frac) { // frac -> val
        if ((frac == 0) && (clipZero == 'true'))
            return 0;
        else
            return (minValue * Math.pow(valueRatio, frac));
    }

    function logValRev (val) { // val -> frac
        if ((frac == 0) && (clipZero == 'true'))
            return 0;
        else
            return (Math.log(val/minValue)/valueLogRatio);
    }

    slider.setBarSize = function (fraction) {
        // value change triggered by mouse interaction in the gui.
        if (fraction !== oldFraction) {
            oldFraction = fraction;
            let newValue = valFunction(fraction).toFixed(3);
            if (newValue !== oldValue) {
                slider.externalValueChange = false;
                calcBarSize(fraction);
                slider.setAttribute('value', newValue);
                slider.externalValueChange = true;
                oldValue = newValue;
                return newValue;
            }
        }
        return oldValue;
    }

    // Attribute change handler


    // flag indicating whether a Value Change is triggered by an
    // external program or by mouse interaction/from multislider. In
    // case it is triggered by an external program (via setAttribute),
    // the value has to be reverse calculated into a slider fraction
    // and no valuechange event is generated.

    slider.externalValueChange = true;

    // override setAttribute

    const mySetAttribute = slider.setAttribute;

    slider.setAttribute = function (key, value) {
        mySetAttribute.call(slider, key, value);
//        console.log('attribute change: ' + key);
        if (key == 'value') {
//            console.log("val-change: " + value + ", oldValue: " + oldValue + ", external: " + slider.externalValueChange);
            if (slider.externalValueChange) {
                if (value != oldValue) {
                    oldValue = parseFloat(value);
                    let fraction = (oldValue-minValue) / (maxValue-minValue);
//                    console.log("value " + oldValue + ", minValue: " + minValue + ", maxValue: " + maxValue + ", fraction: " + fraction);
                    calcBarSize(fraction);
                }
            }
            else
                $(slider).trigger("data", { value: parseFloat(value), idx: idx });
        }
    }
    
    // setup routines

    function setSliderBarStyle () {
        sliderBar.style.position = 'absolute';
        sliderBar.style.backgroundColor = barColor;
        sliderBar.style.border = 'none';
        sliderBar.style.borderRadius = 'inherit';
    }
    
    function setDirection () {
        direction = slider.getAttribute("direction") || 'up';
        thumbWidth = 0; // will get reset below in case thumb == 'true';
        slider.style.setProperty('display', 'flex');
        sliderBar.style.position = 'relative';
        switch (direction) {
        case 'right':
            slider.style.setProperty('flex-direction', 'row-reverse');
            sliderBar.style.width = (getValFraction(value) * sliderWidth) + 'px';
            sliderBar.style.left = '0px';
            sliderBar.style.right = '';
            sliderBar.style.top = '';
            sliderBar.style.bottom = '0px';
            if (thumb == 'true') {
                thumbWidth = '0.15em';
                sliderBar.style.borderLeft = 'none';
                sliderBar.style.borderRight = (sliderWidth/41) + 'px solid ' + thumbColor;
                sliderBar.style.borderTop = 'none';
                sliderBar.style.borderBottom = 'none';
            }
            calcBarSize = calcBarWidth;
            getFraction = getXFraction;
            break;
        case 'left':
            slider.style.setProperty('flex-direction', 'row');
            sliderBar.style.width = (getValFraction(value) * sliderWidth) + 'px';
            sliderBar.style.left = '';
            sliderBar.style.right = '0px';
            sliderBar.style.top = '';
            sliderBar.style.bottom = '0px';
            if (thumb == 'true') {
                thumbWidth = '0.15em';
                sliderBar.style.borderLeft = (sliderWidth/41) + 'px solid ' + thumbColor;
                sliderBar.style.borderRight = 'none';
                sliderBar.style.borderTop = 'none';
                sliderBar.style.borderBottom = 'none';
            }
            calcBarSize = calcBarWidth;
            getFraction = getXFractionRev;
            break;
        case 'down':
            slider.style.setProperty('flex-direction', 'column');
            sliderBar.style.height = (getValFraction(value) * sliderHeight) + 'px';
            sliderBar.style.left = '0px';
            sliderBar.style.right = '';
            sliderBar.style.top = '0px';
            sliderBar.style.bottom = '';
            if (thumb == 'true') {
                thumbWidth = '0.15em';
                sliderBar.style.borderLeft = 'none';
                sliderBar.style.borderRight = 'none';
                sliderBar.style.borderTop = 'none';
                sliderBar.style.borderBottom = (sliderHeight/41) + 'px solid ' + thumbColor;
            }
            calcBarSize = calcBarHeight;
            getFraction = getYFractionRev;
            break;
        default: // 'up'
            slider.style.setProperty('flex-direction', 'column-reverse');
            sliderBar.style.height = (getValFraction(value) * sliderHeight) + 'px';
            sliderBar.style.left = '0px';
            sliderBar.style.right = '';
            sliderBar.style.top = '';
            sliderBar.style.bottom = '0px';
            if (thumb == 'true') {
                thumbWidth = '0.15em';
                sliderBar.style.borderLeft = 'none';
                sliderBar.style.borderRight = 'none';
                sliderBar.style.borderTop = (sliderHeight/41) + 'px solid ' + thumbColor;
                sliderBar.style.borderBottom = 'none';
            }
            calcBarSize = calcBarHeight;
            getFraction = getYFraction;
        }
    }

    function setSliderValue () {
        if (maxValue >= minValue)
            value = clamp(parseFloat((slider.getAttribute("value")) || 0.0 ), minValue, maxValue);
        else
            value = clamp(parseFloat((slider.getAttribute("value")) || 0.0 ), maxValue, minValue);
    }

    function setMinMaxMapping () {
        minValue      = parseFloat(slider.getAttribute("min")) || 0.0;
        maxValue      = parseFloat(slider.getAttribute("max")) || 1.0;
        mapping       = slider.getAttribute("mapping") || 'lin';
        if (mapping == 'log') {
            if ((minValue == 0) && (maxValue == 0)) {
                minValue = 0.01;
                maxValue = 1;
                slider.setAttribute('min', minvalue);
                slider.setAttribute('max', maxValue);
            }
            else {
                if (minValue == 0) {
                    minValue = maxValue / 100;
                    slider.setAttribute('min', minValue);
                }
                else {
                    if (maxValue == 0) {
                        maxValue = minValue / 100;
                        slider.setAttribute('max', maxValue);
                    }
                }
            }
            valueRatio = maxValue/minValue;
            valueLogRatio = Math.log(valueRatio);
            valFunction = logVal;
            valFunctionRev = logValRev;
        }
        else { // linear mapping
            valueRange = maxValue-minValue;
            valFunction = linVal;
            valFunctionRev = linValRev;
        }
        setSliderValue();
        setDirection();
    }

    // Mouse Event Handlers
    
    var moved = false;
    var oldFraction = false;
    var oldValue = false;
    var oldBarSize = false;
    
    function mouseDownListener (event) {
        moved = false;
        oldFraction = false;
        mouseMoveListener(event);
        document.addEventListener('mousemove', mouseMoveListener);
        document.addEventListener('mouseup', mouseUpListener);
    }

    function mouseMoveListener (event) {
        moved = true;
        slider.setBarSize(getFraction(event));
    }
    
    function mouseUpListener (event){
        document.removeEventListener('mousemove', mouseMoveListener);
        document.removeEventListener('mouseup', mouseUpListener);
    }

    slider.removeMouseDownListener = function () {
        slider.removeEventListener('mousedown', mouseDownListener);
    }

    slider.dispatchValChangeEvent = function () {
//        console.log('value changed');
        slider.dispatchEvent(valChangeEvent);
    }

// initialization

    function initSlider () {
        setSliderBarStyle();
        sliderHeight = parseFloat(style.height.match(pxRegex)[1]);
        sliderWidth = parseFloat(style.width.match(pxRegex)[1]);
        setMinMaxMapping(sliderBar);
        slider.addEventListener('mousedown', mouseDownListener)
        slider.externalValueChange = true;
    }

    initSlider();
}


class MultiSliderElement extends HTMLElement {
//  static observedAttributes = ["color", "size"];

    constructor() {
        // Always call super first in constructor
        super();
//        console.log("o-multislider constructed: " + this );
    }

    connectedCallback() {
//        console.log("o-multislider added to page: " + this );
        multislider(this);
    }

    disconnectedCallback() {
        $(myMultiSlider).trigger("data", { close: true });
//        console.log("o-multislider removed from page.");
    }

    adoptedCallback() {
//        console.log("o-multislider moved to new page.");
    }

    attributeChangedCallback(name, oldValue, newValue) {
        console.log(`Attribute ${name} has changed.`);
    }
}

customElements.define("o-multislider", MultiSliderElement );

function multislider(elem, config) {
    var thumb         =  'false';
    const pxRegex = /([0-9]+).*/
    var colors;
    var valueRange;
    var idx;
    var multislider = elem;
    var val = multislider.getAttribute('value');
    var offsetTop = multislider.offsetTop;
    var offsetLeft = multislider.offsetLeft;
    var multisliderHeight, multisliderWidth;
    var minValue, maxValue, value;
    var mapping;
    var direction;
    var clipZero = multislider.getAttribute('clip-zero');
    var mouseDownListener, mouseMoveListener;
    var sliders;
    var sliderType; // mvslider or mhslider, depending on direction
    var getFraction, getIdxFraction;  // functions for calculating the
                                      // Fractions on mousemove depending on
                                      // direction of the sliders.
    var innerBorder;     // we set one of the borders between the
                         // sliders to none except for the border of
    // the first slider. Depending on the direction of the sliders,
    // this is either border-top or border-left.
    
    var style = window.getComputedStyle(multislider, null);

    var numSliders = multislider.getAttribute('num-sliders');
//    var colors = JSON.parse(multislider.getAttribute('colors'));
    if (multislider.getAttribute('colors'))
        colors = multislider.getAttribute('colors').split(',');
    else colors = ['black'];

    console.log('colors: ', colors);
    var numColors = colors.length;
    
    function clamp(number, min, max) {
        return Math.max(min, Math.min(number, max));
    }

    function disableDrag (elem) {
        elem.ondragstart = () => { return false; }
    }
    
    function initSlider (slider) {
        slider.setAttribute('class', sliderType);
        slider.setAttribute('style', 'border: 1px solid black;flex: 1 1 auto;');
        slider.setAttribute('min', minValue);
        slider.setAttribute('max', maxValue);
        slider.setAttribute('val', val);
        slider.setAttribute('clip-zero', clipZero);
        slider.setAttribute('mapping', mapping);
        slider.setAttribute('direction', direction);
        slider.style.setProperty('background-color', 'transparent');
        //        slider.style.setProperty('--thumb-color', 'black');
        slider.style.width = '';
        slider.style.height = '';
    }

    function createSliders (num) {
        let sliders = new Array(num);
        let currSlider;
        console.log('createSliders');
        for (let i = 0; i < num; i++) {
            currSlider = document.createElement('div');
            initSlider(currSlider);
            currSlider.setAttribute('idx', i);
            multislider.appendChild(currSlider);
            console.log("color: " + colors[i%numColors]);
            console.log("colors: ", colors);
            currSlider.style.setProperty('--bar-color', colors[i%numColors]);
            if (i > 0) currSlider.style.setProperty(innerBorder, 'none');
            sliders[i] = currSlider;
            mslider(currSlider, i, numSliders);
            currSlider.removeMouseDownListener();
        }
        return sliders;
    }
        
    function setMinMaxMapping() {
        minValue      = parseFloat(multislider.getAttribute('min')) || 0.0;
        maxValue      = parseFloat(multislider.getAttribute('max')) || 1.0;
        mapping       = multislider.getAttribute('mapping') || 'lin';
        if (mapping == 'log') {
            if ((minValue == 0) && (maxValue == 0)) {
                minValue = 0.01;
                maxValue = 1;
                multislider.setAttribute('min', minvalue);
                multislider.setAttribute('max', maxValue);
            }
            else {
                if (minValue == 0) {
                    minValue = maxValue / 100;
                    multislider.setAttribute('min', minValue);
                }
                else {
                    if (maxValue == 0) {
                        maxValue = minValue / 100;
                        multislider.setAttribute('max', maxValue);
                    }
                }
            }
            valueRatio = maxValue/minValue;
        }
        else {
            valueRange = maxValue-minValue;
        }
        setDirection();
    }
    
    function setSliderHeightVal () {
        multisliderHeight = parseFloat(style.height.match(pxRegex)[1]);
    }

    function setSliderWidthVal () {
        multisliderWidth = parseFloat(style.width.match(pxRegex)[1]);
    }

    // Utils
    
    function getValFraction (val) {
        return ((val - minValue) / valueRange);
    }

    function getYFraction (event) {
        let boundingClientRect = multislider.getBoundingClientRect();
        let localYFrac = (boundingClientRect.height + boundingClientRect.top - event.clientY) / boundingClientRect.height;
        return clamp(localYFrac, 0, 1);
    }

    function getXFraction (event) {
        let boundingClientRect = multislider.getBoundingClientRect();
        let localXFrac = (event.clientX - boundingClientRect.left) / boundingClientRect.width;
        return clamp(localXFrac, 0, 1);
    }

    function getYFractionRev (event) {
        return (1 - getYFraction(event));
    }

    function getXFractionRev (event) {
        return (1 - getXFraction(event));
    }

    function setDirection () {
        direction = multislider.getAttribute('direction') || 'up';
        switch (direction) {
        case 'right':
            sliderType = 'mhslider';
            innerBorder = 'border-top';
            multislider.style.flexDirection = 'column';
            getFraction = getXFraction;
            getIdxFraction = getYFractionRev;
            break;
        case 'left':
            sliderType = 'mhslider';
            innerBorder = 'border-top';
            multislider.style.flexDirection = 'column';
            getFraction = getXFractionRev;
            getIdxFraction = getYFractionRev;
            break;
        case 'down':
            sliderType = 'mvslider';
            innerBorder = 'border-left';
            getFraction = getYFractionRev;
            getIdxFraction = getXFraction;
            break;
        default: // 'up'
            sliderType = 'mvslider';
            innerBorder = 'border-left';
            getFraction = getYFraction;
            getIdxFraction = getXFraction;
        }
    }

    // mouse handling
    
    var moved = false;
    var lastIdx = false;
    var lastFraction = false;
    
    function mouseDownListener (event) {
        moved = false;
        let idxFraction = getIdxFraction(event);
        let valFraction = getFraction(event);
        idx = Math.floor(idxFraction*numSliders);
        if (idx >= numSliders) idx = numSliders - 1;
        sliders[idx].setBarSize(valFraction);
        lastFraction = valFraction;
        lastIdx = idx;
        document.addEventListener('mousemove', mouseMoveListener);
        document.addEventListener('mouseup', mouseUpListener);
    }

    function interpolateSetBarSize (idx, fraction) {
        let dIdx = idx - lastIdx;
        let dFraction = fraction - lastFraction;
        if (lastIdx && (idx != lastIdx)) {
            if (idx > lastIdx) {
                for (let i = idx; i > lastIdx;i--) {
                    let f = fraction + ((i-idx)/dIdx * dFraction);
                    sliders[i].setBarSize(f);
                }
            }
            else {
                for (let i = idx; i < lastIdx;i++) {
                    let f = fraction + ((i-idx)/dIdx * dFraction);
                    sliders[i].setBarSize(f);
                }
            }
        }
        else sliders[idx].setBarSize(fraction);
    }
    
    function mouseMoveListener (event) {
        moved = true;
        let valFraction = getFraction(event);
        let idxFraction = getIdxFraction(event);
        idx = Math.floor(idxFraction*numSliders);
        if (idx >= numSliders) idx = numSliders - 1;
        if ((idx != lastIdx) || (valFraction != lastFraction)) {
            interpolateSetBarSize(idx, valFraction);
            lastFraction = valFraction;
            lastIdx = idx;
        }
    }

    function mouseUpListener (event){
        document.removeEventListener('mousemove', mouseMoveListener);
        document.removeEventListener('mouseup', mouseUpListener);
    }

    function init () {
        disableDrag(multislider);
        setSliderHeightVal();
        setSliderWidthVal();
        setMinMaxMapping();
        setDirection();
        sliders = createSliders(numSliders, multislider);
        multislider.sliders = sliders;
        multislider.colors = colors;
        multislider.addEventListener('mousedown', mouseDownListener);
    }

    init();
}

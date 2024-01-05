//
// slider.js
//
// definition of slider mouse and event handling in the
// client. slider() gets called with the div being the track of the
// slider. It creates and initializes the div for the sliderbar with
// the thumb as border. All information needed for proper
// initialization needs to be contained in the attributes of the
// slider track div before calling slider().
//
// WARNING: Currently only changing the data-value attribute after
// initialization is supported. All other attribute or style changes
// after initialization probably have no or detrimental effects.
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
function slider(elem, config){
//    var barColor      = config.barColor || 'transparent';
//    var thumbColor    = config.thumbColor || 'black';
    var thumb         = config.thumb || 'true';

    var slider;
    if (elem.nodeType == undefined)
        slider = elem.get(0);
    else
        slider = elem;

    function disableDrag (elem) {
       // elem.ondragstart = undefined
    }

    var sliderBar = document.createElement("div");
    slider.appendChild(sliderBar);

    disableDrag(slider);
    disableDrag(sliderBar);

    const valChangeEvent = new Event("valuechange");
    const pxRegex = /([0-9]+).*/
    var offsetTop = slider.offsetTop;
    var offsetLeft = slider.offsetLeft;
    var sliderHeight;
    var sliderWidth;
    var colors;
    var minValue;
    var maxValue;
    var value;
    var mapping;
    var direction;
    var clipZero = slider.getAttribute('data-clip-zero');
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
    var thumbColor = style.getPropertyValue('--thumb-color');
    var barColor = style.getPropertyValue('--bar-color');

    var getFraction;
    var calcBarSize;;

    function clamp(number, min, max) {
        return Math.max(min, Math.min(number, max));
    }

    // Utils
    
    function getValFraction (val) {
        return ((val - minValue) / valueRange);
    }

    function getYFraction (event) {
        let localYFrac = (sliderHeight + slider.getBoundingClientRect().top - event.clientY) / sliderHeight;
        return clamp(localYFrac, 0, 1);
    }

    function getYFractionRev (event) {
        return (1 - getYFraction(event));
    }

    function getXFraction (event) {
        let localXFrac = ((event.clientX - slider.getBoundingClientRect().left)) / sliderWidth;
        return clamp(localXFrac, 0, 1);
    }

    function getXFractionRev (event) {
        return (1 - getXFraction(event));
    }

    function calcBarHeight (YFraction) {
        let newBarSize = (YFraction * (sliderHeight - thumbWidth)) + 'px';
        if (newBarSize != oldBarSize) {
            oldBarSize = newBarSize;
            sliderBar.style.height = newBarSize;
        }
    }

    function calcBarWidth (XFraction) {
        let newBarSize = (XFraction * (sliderWidth - thumbWidth)) + 'px';
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
                slider.setAttribute('data-val', newValue);
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
        if (key == 'data-val') {
//            console.log("val-change: " + value + ", oldValue: " + oldValue + ", external: " + slider.externalValueChange);
            if (slider.externalValueChange) {
                if (value != oldValue) {
                    oldValue = value;
                    let fraction = valFunctionRev(value);
                    calcBarSize(fraction);
                }
            }
            else 
                slider.dispatchEvent(valChangeEvent);
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
        direction = slider.getAttribute("data-direction") || 'up';
        thumbWidth = 0.5; // will get reset below in case thumb == 'true';
        switch (direction) {
        case 'right':
            sliderBar.style.height = '100%';
            sliderBar.style.width = (getValFraction(value) * sliderWidth) + 'px';
            sliderBar.style.left = '0px';
            sliderBar.style.right = '';
            sliderBar.style.top = '';
            sliderBar.style.bottom = '0px';
            if (thumb == 'true') {
                thumbWidth = 1.5;
                sliderBar.style.borderLeft = 'none';
                sliderBar.style.borderRight = (sliderWidth/41) + 'px solid ' + thumbColor;
                sliderBar.style.borderTop = 'none';
                sliderBar.style.borderBottom = 'none';
            }
            calcBarSize = calcBarWidth;
            getFraction = getXFraction;
            break;
        case 'left':
            sliderBar.style.height = '100%';
            sliderBar.style.width = (getValFraction(value) * sliderWidth) + 'px';
            sliderBar.style.left = '';
            sliderBar.style.right = '0px';
            sliderBar.style.top = '';
            sliderBar.style.bottom = '0px';
            if (thumb == 'true') {
                thumbWidth = 1.5;
                sliderBar.style.borderLeft = (sliderWidth/41) + 'px solid ' + thumbColor;
                sliderBar.style.borderRight = 'none';
                sliderBar.style.borderTop = 'none';
                sliderBar.style.borderBottom = 'none';
            }
            calcBarSize = calcBarWidth;
            getFraction = getXFractionRev;
            break;
        case 'down':
            sliderBar.style.width = '100%';
            sliderBar.style.height = (getValFraction(value) * sliderHeight) + 'px';
            sliderBar.style.left = '0px';
            sliderBar.style.right = '';
            sliderBar.style.top = '0px';
            sliderBar.style.bottom = '';
            if (thumb == 'true') {
                thumbWidth = 1.5;
                sliderBar.style.borderLeft = 'none';
                sliderBar.style.borderRight = 'none';
                sliderBar.style.borderTop = 'none';
                sliderBar.style.borderBottom = (sliderHeight/41) + 'px solid ' + thumbColor;
            }
            calcBarSize = calcBarHeight;
            getFraction = getYFractionRev;
            break;
        default: // 'up'
            sliderBar.style.width = '100%';
            sliderBar.style.height = (getValFraction(value) * sliderHeight) + 'px';
            sliderBar.style.left = '0px';
            sliderBar.style.right = '';
            sliderBar.style.top = '';
            sliderBar.style.bottom = '0px';
            if (thumb == 'true') {
                thumbWidth = 1.5;
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
            value = clamp(parseFloat((slider.getAttribute("data-val")) || 0.0 ), minValue, maxValue);
        else
            value = clamp(parseFloat((slider.getAttribute("data-val")) || 0.0 ), maxValue, minValue);
    }

    function setMinMaxMapping () {
        minValue      = parseFloat(slider.getAttribute("data-min")) || 0.0;
        maxValue      = parseFloat(slider.getAttribute("data-max")) || 1.0;
        mapping       = slider.getAttribute("data-mapping") || 'lin';
        if (mapping == 'log') {
            if ((minValue == 0) && (maxValue == 0)) {
                minValue = 0.01;
                maxValue = 1;
                slider.setAttribute('data-min', minvalue);
                slider.setAttribute('data-max', maxValue);
            }
            else {
                if (minValue == 0) {
                    minValue = maxValue / 100;
                    slider.setAttribute('data-min', minValue);
                }
                else {
                    if (maxValue == 0) {
                        maxValue = minValue / 100;
                        slider.setAttribute('data-max', maxValue);
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
        clipZero = slider.getAttribute("data-clip-zero") || 'false';
        setMinMaxMapping(sliderBar);
        slider.addEventListener('mousedown', mouseDownListener)
        slider.externalValueChange = true;
    }

    initSlider();
}

function toggle (elem, config) {

    var myToggle;
    
    if (elem.nodeType == undefined)
        myToggle = elem.get(0);
    else
        myToggle = elem;
    
    const valChangeEvent = new Event("valuechange");
    var colorOff         = config.colorOff || "black";
    var backgroundOff    = config.backgroundOff || "white";
    var labelOff         = config.labelOff || '';
    var valueOff         = config.valueOff || '0';
    var colorOn          = config.colorOn || "black";
    var backgroundOn     = config.backgroundOn || 'rgba(120,120,120,1.0)';
    var labelOn          = config.labelOn || '';
    var valueOn          = config.valueOn || '1';

    myToggle.externalValueChange = false;

    // override setAttribute
    const mySetAttribute = myToggle.setAttribute;

    myToggle.colorOff = colorOff;
    myToggle.backgroundOff = backgroundOff;
    myToggle.labelOff = labelOff;
    myToggle.valueOff = valueOff;
    myToggle.colorOn = colorOn;
    myToggle.backgroundOn = backgroundOn;
    myToggle.labelOn = labelOn;
    myToggle.valueOn = valueOn;
    
    myToggle.setAttribute = function (key, value) {
        if (key == 'data-val') {
            if (myToggle.externalValueChange == true) {
                value = parseFloat(value).toFixed(0);
                if (value != valueOff) value = valueOn;
            }
            mySetAttribute.call(myToggle, key, value);
            drawToggle(value);
            if (myToggle.externalValueChange == false) {
                myToggle.dispatchEvent(valChangeEvent);
                myToggle.externalValueChange = true;
            }
        }
    }
    
    var  drawToggle = function (val) {
        if (val == valueOn) {
            myToggle.textContent = labelOn;
            myToggle.style.color = colorOn;
            myToggle.style.background = backgroundOn;
        }
        else {
            myToggle.textContent = labelOff;
            myToggle.style.color = colorOff;
            myToggle.style.background = backgroundOff;
        }
    }

    function mouseDownListener (event) {
        myToggle.externalValueChange = false;
        let val = (myToggle.getAttribute('data-val') == myToggle.valueOff)? valueOn : valueOff;
        myToggle.setAttribute('data-val', val);
    }

    myToggle.removeMouseDownListener = () => {
        myToggle.removeEventListener('mousedown', mouseDownListener);
    }

    myToggle.draw = drawToggle;

    function disable () { return false };
    
    function init () {
        myToggle.colorOff = colorOff;
        myToggle.backgroundOff = backgroundOff;
        myToggle.labelOff = labelOff;
        myToggle.valueOff = valueOff;
        myToggle.colorOn = colorOn;
        myToggle.backgroundOn = backgroundOn;
        myToggle.labelOn = labelOn;
        myToggle.valueOn = valueOn;
        myToggle.ondragstart = () => { return false; }
        myToggle.addEventListener('mousedown', mouseDownListener);
        myToggle.onselectstart = disable;
        // myToggle.addEventListener('dblclick', function(event) {
        //     event.preventDefault();
        //     event.stopPropagation();
        // }, true);
        let val = parseFloat(myToggle.getAttribute('data-val')).toFixed(0);
        if ((val != valueOff) && (val != valueOn)) {
            val = valueOff;
            mySetAttribute.call(myToggle, 'data-val', val);
        }
        drawToggle(val);
        myToggle.externalValueChange = true;
    }

    init();
}

function radio (elem, config) {

    var myRadio = elem.get(0);

    var colorOff      = config.colorOff                  || ['black'];
    var backgroundOff = config.backgroundOff             || ['white'];
    var labelOff      = config.labelOff                  || ['Off'];
    var colorOn       = config.colorOn                   || ['black'];
    var backgroundOn  = config.backgroundOn              || ['rgba(120,120,120,1.0)'];
    var labelOn       = config.labelOn                   || ['On'];
    var direction     = config.direction                 || 'right';
    var numButtons           = myRadio.getAttribute('data-num') || ['1.0'];

    var lenColOn = colorOn.length;
    var lenColOff = colorOff.length;
    var lenBgOn = backgroundOn.length;
    var lenBgOff = backgroundOff.length;
    var lenLbOn = labelOn.length;
    var lenLbOff = labelOff.length;
    
    const pxRegex = /([0-9]+).*/
    const valChangeEvent = new Event("valuechange");
    var innerBorder, mouseDownListener, oldValue;

    var getFraction;

    myRadio.externalValueChange = true;
    
    var style = window.getComputedStyle(myRadio, null);

    myRadio.height = parseFloat(style.height.match(pxRegex)[1]);
    myRadio.width = parseFloat(style.width.match(pxRegex)[1]);

    // Utils
    
    function clamp(number, min, max) {
        return Math.max(min, Math.min(number, max));
    }

    function makeToggle () {
        let div = document.createElement('div');
        div.setAttribute('class', 'mvradio');
        div.setAttribute('style', 'border: 1px solid black;flex: 1 0 auto;position: relative;min-width 0; min-height: 0');
        div.style.width = '';
        div.style.height = '';
        div.style.flex = '1 0 auto';
        div.style.display = 'flex';
        div.style.justifyContent = 'center';
        div.style.pointerEvents = 'none';
        return div;
    }

    function createToggles (num, parent) {
        let toggles = new Array(num);
        let currSlider;
        let idx;
        for (let i = 0; i < num; i++) {
            let currToggle = makeToggle();
            parent.appendChild(currToggle);
            idx = ((direction == 'up') || (direction == 'left'))? (num - i - 1) : i;
            currToggle.setAttribute('data-idx', idx);
            if (i > 0) currToggle.style.setProperty(innerBorder, 'none');
            toggles[idx] = currToggle;
            toggle(currToggle, { 'colorOff': colorOff[idx%lenColOff],
                                 'backgroundOff': backgroundOff[idx%lenBgOff],
                                 'labelOff': labelOff[idx%lenLbOff],
                                 'colorOn': colorOn[idx%lenColOn],
                                 'backgroundOn': backgroundOn[idx%lenBgOn],
                                 'labelOn': labelOn[idx%lenLbOn] });
            if (idx == oldValue) {
                currToggle.setAttribute('data-val', 1);
                currToggle.draw(1);
            }
            else {
                currToggle.setAttribute('data-val', 0);
                currToggle.draw(0);
            }
            currToggle.removeMouseDownListener();
        }
        return toggles;
    }

    function getYFraction (event) {
        let localYFrac = (myRadio.height + myRadio.offsetTop - event.clientY) / myRadio.height;
        return clamp(localYFrac, 0, 1);

    }

    function getYFractionRev (event) {
         return (1 - getYFraction(event));
    }

    function getXFraction (event) {
        let localXFrac = ((event.clientX - myRadio.offsetLeft)) / myRadio.width;
        return clamp(localXFrac, 0, 1);
    }

   function getXFractionRev (event) {
         return (1 - getXFraction(event));
    }
    
    function mouseDownListener (event) {
        moved = false;
        let Fraction = getFraction(event);
        idx = clamp(Math.floor(Fraction*numButtons), 0, numButtons - 1);
        myRadio.externalValueChange = false;
        myRadio.setAttribute('data-val', idx);
    }

    function drawRadio (val) {
        myRadio.toggles[oldValue].setAttribute('data-val', 0);
        myRadio.toggles[oldValue].draw(0);
        myRadio.toggles[val].setAttribute('data-val', 1);
        myRadio.toggles[val].draw(1);
        oldValue = val;
    }
    
    // overwrite setAttribute

    const mySetAttribute = myRadio.setAttribute;
    
    myRadio.setAttribute = function (key, value) {
        if (key == 'data-val') {
            if (myRadio.externalValueChange == true) {
                value = clamp(parseFloat(value).toFixed(0), 0, numButtons - 1);
            }
            mySetAttribute.call(myRadio, key, value);
            drawRadio(value);
            if (myRadio.externalValueChange == false) {
                myRadio.dispatchEvent(valChangeEvent);
                myRadio.externalValueChange = true;
            }
        }
    }

    function setDirection () {
        switch (direction) {
        case 'down':
            sliderType = 'vradio';
            getFraction = getYFractionRev;
            innerBorder = 'border-top';
            myRadio.style.flexDirection = "column";
            break;
        case 'up':
            sliderType = 'vradio';
            getFraction = getYFraction;
            innerBorder = 'border-top';
            myRadio.style.flexDirection = "column";
            break;
        case 'left':
            sliderType = 'hradio';
            getFraction = getXFractionRev;
            innerBorder = 'border-left';
            break;
        default: // right
            sliderType = 'hradio';
            getFraction = getXFraction;
            innerBorder = 'border-left';
            break;
        }
    }

    function disable () { return false };


    function init () {
        setDirection();
        oldValue = clamp(parseFloat(myRadio.getAttribute('data-val')).toFixed(0), 0, numButtons - 1);
        myRadio.toggles = createToggles(numButtons, myRadio);
        myRadio.style.display = 'flex';
        myRadio.addEventListener('mousedown', mouseDownListener);
        myRadio.onselectstart = disable;
    }
    
    init();
}

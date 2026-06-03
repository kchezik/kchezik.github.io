function Vector(x, y, z) {
    this.x = x;
    this.y = y;
    this.z = z;
 
    this.set = function (x, y) {
        this.x = x;
        this.y = y;
    };
}
 
function PointCollection() {
    this.mousePos = new Vector(0, 0);
    this.pointCollectionX = 0;
    this.pointCollectionY = 0;
    this.points = [];
 
    this.update = function () {
        for (var i = 0; i < this.points.length; i++) {
            var point = this.points[i];
 
            var dx = this.mousePos.x - point.curPos.x;
            var dy = this.mousePos.y - point.curPos.y;
            var dd = (dx * dx) + (dy * dy);
            var d = Math.sqrt(dd);
 
            point.targetPos.x = d < 150 ? point.curPos.x - dx : point.originalPos.x;
            point.targetPos.y = d < 150 ? point.curPos.y - dy : point.originalPos.y;
 
            point.update();
        }
    };
 
    this.shake = function () {
        var randomNum = Math.floor(Math.random() * 5) - 2;
 
        for (var i = 0; i < this.points.length; i++) {
            var point = this.points[i];
            var dx = this.mousePos.x - point.curPos.x;
            var dy = this.mousePos.y - point.curPos.y;
            var dd = (dx * dx) + (dy * dy);
            var d = Math.sqrt(dd);
            if (d < 50) {
                this.pointCollectionX = Math.floor(Math.random() * 5) - 2;
                this.pointCollectionY = Math.floor(Math.random() * 5) - 2;
            }
            point.draw(bubbleShape, this.pointCollectionX, this.pointCollectionY);
        }
    };
 
    this.draw = function (bubbleShape, reset) {
        for (var i = 0; i < this.points.length; i++) {
            var point = this.points[i];
 
            if (point === null)
                continue;
 
            if (window.reset) {
                this.pointCollectionX = 0;
                this.pointCollectionY = 0;
                this.mousePos = new Vector(0, 0);
            }
 
            point.draw(bubbleShape, this.pointCollectionX, this.pointCollectionY, reset);
        }
    };
 
    this.reset = function (bubbleShape) {};
}
 
function Point(x, y, z, size, color) {
    this.curPos = new Vector(x, y, z);
    this.color = color;
 
    this.friction = document.Friction;
    this.rotationForce = document.rotationForce;
    this.springStrength = 0.1;
 
    this.originalPos = new Vector(x, y, z);
    this.radius = size;
    this.size = size;
    this.targetPos = new Vector(x, y, z);
    this.velocity = new Vector(0.0, 0.0, 0.0);
 
    this.update = function () {
        var dx = this.targetPos.x - this.curPos.x;
        var dy = this.targetPos.y - this.curPos.y;
        // Orthogonal vector is [-dy,dx]
        var ax = dx * this.springStrength - this.rotationForce * dy;
        var ay = dy * this.springStrength + this.rotationForce * dx;
 
        this.velocity.x += ax;
        this.velocity.x *= this.friction;
        this.curPos.x += this.velocity.x;
 
        this.velocity.y += ay;
        this.velocity.y *= this.friction;
        this.curPos.y += this.velocity.y;
 
        var dox = this.originalPos.x - this.curPos.x;
        var doy = this.originalPos.y - this.curPos.y;
        var dd = (dox * dox) + (doy * doy);
        var d = Math.sqrt(dd);
 
        this.targetPos.z = d / 100 + 1;
        var dz = this.targetPos.z - this.curPos.z;
        var az = dz * this.springStrength;
        this.velocity.z += az;
        this.velocity.z *= this.friction;
        this.curPos.z += this.velocity.z;
 
        this.radius = this.size * this.curPos.z;
        if (this.radius < 1) this.radius = 1;
    };
 
    this.draw = function (bubbleShape, dx, dy) {
        ctx.fillStyle = this.color;
        if (bubbleShape == "square") {
            ctx.beginPath();
            ctx.fillRect(this.curPos.x + dx, this.curPos.y + dy, this.radius * 1.5, this.radius * 1.5);
        } else {
            ctx.beginPath();
            ctx.arc(this.curPos.x + dx, this.curPos.y + dy, this.radius, 0, Math.PI * 2, true);
            ctx.fill();
        }
    };
}

function makeColor(hslList, fade) {
    var hue = hslList[0] - 17.0 * fade / 1000.0;
    var sat = hslList[1] + 81.0 * fade / 1000.0;
    var lgt = hslList[2] + 58.0 * fade / 1000.0;
    return "hsl(" + hue + "," + sat + "%," + lgt + "%)";
}
 
function phraseToHex(phrase) {
    var hexphrase = "";
    for (var i = 0; i < phrase.length; i++) {
        hexphrase += phrase.charCodeAt(i).toString(16);
    }
    return hexphrase;
}
 
/*function initEventListeners() {
    // Re-trigger drawName on resize so positions recalculate to the new width/height
    $(window).bind('resize', function() {
        if (typeof currentNameString !== 'undefined') {
            drawName(currentNameString, currentColorsArray);
        } else {
            updateCanvasDimensions();
        }
    }).bind('mousemove', onMove);
 
    canvas.ontouchmove = function (e) {
        e.preventDefault();
        onTouchMove(e);
    };
 
    canvas.ontouchstart = function (e) {
        e.preventDefault();
    };
} */

function initEventListeners() {
    $(window).bind('resize', function() {
        // 1. Wipe out existing bubble points immediately to halt the physics engine loop
        if (typeof pointCollection !== 'undefined' && pointCollection) {
            pointCollection.points = [];
        }
        
        // 2. Clear the canvas completely so old visual paths are instantly deleted
        if (typeof ctx !== 'undefined' && ctx && typeof canvasWidth !== 'undefined') {
            ctx.clearRect(0, 0, canvasWidth, canvasHeight);
        }
    
        // 3. Recalculate dynamic boundaries and rebuild the font matrix mapping cleanly
        if (typeof currentNameString !== 'undefined') {
            drawName(currentNameString, currentColorsArray);
        } else {
            updateCanvasDimensions();
        }
    }).bind('mousemove', onMove);

    canvas.ontouchmove = function (e) {
        e.preventDefault();
        onTouchMove(e);
    };
 
    canvas.ontouchstart = function (e) {
        e.preventDefault();
    };
}

 
function updateCanvasDimensions() {
    // Find the parent element (the jumbotron)
    var parent = canvas.parent();
    
    canvas.attr({
        height: parent.innerHeight(),
        width: parent.innerWidth()
    });
    
    canvasWidth = canvas.width();
    canvasHeight = canvas.height();
    draw();
}
 
function onMove(e) {
    if (pointCollection) {
        pointCollection.mousePos.set(e.pageX - canvas.offset().left, e.pageY - canvas.offset().top);
    }
}
 
function onTouchMove(e) {
    if (pointCollection) {
        pointCollection.mousePos.set(e.targetTouches[0].pageX - canvas.offset().left, e.targetTouches[0].pageY - canvas.offset().top);
    }
}
 
function bounceName() {
    shake();
    setTimeout(bounceName, 30);
}
 
function bounceBubbles() {
    draw();
    update();
    setTimeout(bounceBubbles, 30);
}
 
function draw(reset) {
    var tmpCanvas = canvas.get(0);
 
    if (tmpCanvas.getContext === null) {
        return;
    }
 
    ctx = tmpCanvas.getContext('2d');
    ctx.clearRect(0, 0, canvasWidth, canvasHeight);
 
    bubbleShape = typeof bubbleShape !== 'undefined' ? bubbleShape : "circle";
 
    if (pointCollection) {
        pointCollection.draw(bubbleShape, reset);
    }
}
 
function shake() {
    var tmpCanvas = canvas.get(0);
 
    if (tmpCanvas.getContext === null) {
        return;
    }
 
    ctx = tmpCanvas.getContext('2d');
    ctx.clearRect(0, 0, canvasWidth, canvasHeight);
 
    bubbleShape = typeof bubbleShape !== 'undefined' ? bubbleShape : "circle";
 
    if (pointCollection) {
        pointCollection.shake(bubbleShape);
    }
}
 
function update() {
    if (pointCollection)
        pointCollection.update();
}
 
function drawName(name, letterColors) {
    updateCanvasDimensions();
    var g = [];
    
    // --- DYNAMIC SCALING MODIFIER ---
    // Calculates a fluid scale factor based on screen width
    // Ensures a baseline around 0.55 on wide desktops, but scales down cleanly on mobile
    var fontSizeMultiplier = Math.min(0.55, canvasWidth / 2400); 
    
    // Fallback limit so text never gets too microscopic on tiny screens
    if (fontSizeMultiplier < 0.25) fontSizeMultiplier = 0.25;

    var fontHeightMultiplier = fontSizeMultiplier + 0.25;
    
    // START AT ZERO: Track the local coordinates of the letters first
    var textBlockWidth = 0;
    var hexphrase = phraseToHex(name);
 
    function addLetter(cc_hex, ix, letterCols) {
        if (typeof letterCols !== 'undefined') {
            if (Object.prototype.toString.call(letterCols) === '[object Array]' && Object.prototype.toString.call(letterCols) === '[object Array]') {
                letterColors = letterCols;
            }
            if (Object.prototype.toString.call(letterCols) === '[object Array]' && typeof letterCols === "number") {
                letterColors = [letterCols];
            }
        } else {
            letterColors = [];
        }
 
        if (document.alphabet.hasOwnProperty(cc_hex)) {
            var chr_data = document.alphabet[cc_hex].P;
            var bc = letterColors[ix % letterColors.length];
 
            for (var i = 0; i < chr_data.length; ++i) {
                var point = chr_data[i];
 
                // Create points starting at 0 horizontally
                g.push(
                    new Point(
                        point[0] * fontSizeMultiplier + textBlockWidth,
                        point[1] * fontHeightMultiplier,
                        0.0,
                        point[2] * (fontSizeMultiplier + 0.7),
                        makeColor(bc, point[3])
                    )
                );
            }
            textBlockWidth += document.alphabet[cc_hex].W * fontSizeMultiplier;
        }
    }
 
    var col_ix = -1;
    for (var i = 0; i < hexphrase.length; i += 2) {
        var cc_hex = "A" + hexphrase.charAt(i) + hexphrase.charAt(i + 1);
        if (cc_hex != "A20") {
            col_ix++;
            addLetter(cc_hex, col_ix, letterColors);
        } else {
            textBlockWidth += 20 * fontSizeMultiplier; 
        }
    }
 
    // --- CALCULATE PERFECT CENTER BOUNDS ---
    var estimatedTextHeight = 105 * fontHeightMultiplier; 
    var startX = (canvasWidth - textBlockWidth) / 2;
    var startY = ((canvasHeight - estimatedTextHeight) / 2) - (canvasHeight * 0.25);

    // --- APPLY TRANSFORMATION ONCE ---
    for (var j = 0; j < g.length; j++) {
        // Map original tracking targets directly to the screen layout space
        g[j].originalPos.x += startX;
        g[j].originalPos.y += startY;

        // Reset real-time interactive tracking grids
        g[j].targetPos.x = g[j].originalPos.x;
        g[j].targetPos.y = g[j].originalPos.y;

        // Visual drop-in effect coordinates on page initialization 
        g[j].curPos.x = (canvasWidth / 2) + (g[j].originalPos.x - canvasWidth / 2) * 1.016;
        g[j].curPos.y = (canvasHeight / 2 - 105 * 1.7) + (g[j].originalPos.y - canvasHeight / 2);
    }
 
    pointCollection = new PointCollection();
    pointCollection.points = g;
    initEventListeners();
}

 
window.reset = false;
 
$(window).mouseleave(function () {
    window.reset = true;
});
 
$(window).mouseenter(function () {
    window.reset = false;
});
 
var canvas = $("#myCanvas");
var canvasHeight;
var canvasWidth;
var ctx;
var pointCollection;
 
document.rotationForce = 0.0;
document.Friction = 0.85;

var white = [0, 0, 100];
var black = [0, 0, 27];
var red = [0, 100, 63];
var orange = [40, 100, 60];
var green = [75, 100, 40];
var blue = [196, 77, 55];
var purple = [280, 50, 60];
 
setTimeout(updateCanvasDimensions, 30);

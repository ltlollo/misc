// ==UserScript==
// @name        Video Speed
// @namespace   Video
// @version     1.0
// @grant       none
// ==/UserScript==

var fastSpeed = 2.0;
var fastKey = "+";
var slowSpeed = 1.25;
var slowKey = "-";

var setSpeed = function(vEle, vSpeed) {
  vEle.playbackRate = vSpeed;
}

var changeSpeed = function(vSpeed) {
  Array.prototype.forEach.call(
    document.getElementsByTagName("video"),
    function(vEle) {
      setSpeed(vEle, vSpeed);
    }); 
}

var handleKey = function(event) {
  if(event.key === fastKey) {
    changeSpeed(fastSpeed);
  } else if (event.key === slowKey) {
    changeSpeed(slowSpeed);
  }
}

window.addEventListener("play", function() {
  Array.prototype.forEach.call(
    document.getElementsByTagName("video"),
    function(vEle) {
        setSpeed(vEle, fastSpeed);
    });
  document.addEventListener("keypress", handleKey, false)
}, true)


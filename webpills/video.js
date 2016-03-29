// ==UserScript==
// @name        Video Speed
// @namespace   Video
// @version     1.0
// @grant       none
// ==/UserScript==
var fastKey = '+';
var slowKey = '-';
var curr = 1;
var vidSpeeds = [
  1,
  1.25,
  1.5,
  2,
];
var setSpeed = function (vEle, vSpeed) {
  currSpeed = vSpeed;
  vEle.playbackRate = vSpeed;
}
var changeSpeed = function (vSpeed) {
  Array.prototype.forEach.call(document.getElementsByTagName('video'), function (vEle) {
    setSpeed(vEle, vSpeed);
  });
}
var handleKey = function (event) {
  if (event.key === fastKey) {
    changeSpeed(vidSpeeds[++curr % vidSpeeds.length]);
  } else if (event.key === slowKey) {
    changeSpeed(vidSpeeds[--curr % vidSpeeds.length]);
  }
}
window.addEventListener('load', function () {
  document.addEventListener('keypress', handleKey, false);
}, false)
window.addEventListener('canplay', function () {
  Array.prototype.forEach.call(document.getElementsByTagName('video'), function (vEle) {
    setSpeed(vEle, vidSpeeds[curr]);
  });
}, true)

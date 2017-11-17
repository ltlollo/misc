javascript:(function(){
  lns = document.querySelectorAll('p.altte');
  if (lns.length) {
    for (i = 0; i<lns.length; i++) {
        lns[i].remove();
    }
    return;
  }
  lns = document.querySelectorAll('div>a>div>img');
  for (i = 0; i<lns.length; i++) {
	if (!lns[i].getAttribute('alt')) {
		continue;
	}
    var p = document.createElement('p');
    var a = document.createAttribute("class");
    a.value = "altte";  
    p.setAttributeNode(a);
    p.style.backgroundColor = "cyan";
    p.style.fontSize = "medium";
    p.innerText = lns[i].getAttribute('alt');
    lns[i].parentNode.parentNode.parentNode.appendChild(p);
  }
}());

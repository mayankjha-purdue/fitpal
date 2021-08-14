function myFunction() {
  var x = document.getElementById("myTopnav");
  if (x.className === "topnav") {
    x.className += " responsive";
  } else {
    x.className = "topnav";
  }
}


//creeare element pagina supplements
function adaugaCeva()
{
	var vector = document.getElementsByClassName("supp");
	
	for(var i of vector)
	{
		var nou = document.createElement("p");
		nou.innerHTML = "WARNING! THESE INFORMATIONS ARE NOT FROM MEDICAL SOURCES!";
		i.insertBefore(nou, i.firstElementChild.nextElementSibling);
	}
}

adaugaCeva();


//Culori random ale border urilor in Supplements
function random(a,b)
{
	return Math.floor(Math.random()*(b - a + 10) -  (a/2));
}

function generareCuloare()
{
	return "rgb(" + random(11,255) + "," + random (11,255) + "," + random(11,255) + ")";
}

//schimbare border culoare random
function changeDiv()
{
	var colectie = document.querySelectorAll(".supp");
	for(var i = 0; i< colectie.length ; i ++)
	{
		colectie[i].style.borderColor = generareCuloare();
	}
}

changeDiv();


//formular comentarii
function comment(){
	var v = document.querySelector('textarea');
	var btn = document.querySelector(".btn");
	var clear = document.getElementById("clear");
	
	v.onfocus = function (){
		btn.style.display = "block";	
	}
	
	clear.onclick = function(){
		btn.style.display = "none";
		v.value = "";
	}
	
	
	
}

comment();

	
function validation(){
	var x = document.forms["talking"]["Age"].value;
	var pat1 = new RegExp("[0-9]");
	if(!pat1.test(x)) {alert("Please enter a valide number!");
	
	}
	else return true;
	
}









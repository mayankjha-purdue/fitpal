$(document).ready(function(){
	$('td').click(function(){
		$(this).parent().siblings().fadeOut(200);
		$(this).siblings().fadeOut(200);
		$(this).height("450px");	
		$(this).width("100%");	
		$(this).children().height("480px");
		$(this).children().width("100%");
		$('table').height("520px");
		$('.sideshift').height("75.6em");
		$('.divex').append('<img id="backit" src="http://www.freestockphotos.biz/pictures/3/3669/arrow.png"/>');
		var tfg= $(this);
		$(this).children("p").css({"margin-left":"460px","font-size":"1em"});

	$('#backit').click(function(){
		$(tfg).height("250px");	
		$(tfg).width("24%");	
		$('.sideshift').height("104em");
		$(tfg).children().height("180px");
		$(tfg).children().width("88%");	
		$(tfg).parent().siblings().fadeIn(200);
		$(tfg).siblings().fadeIn(200);	
		$('#backit').remove();	
		$(tfg).children("p").css({"margin-left":"14px","font-size":"13px"});
		});
	});
});

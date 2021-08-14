$(document).ready(function(){
	$('td').click(function(){
		$(this).parent().siblings().fadeOut(200);
		$(this).siblings().fadeOut(200);
		$(this).height("450px");	
		$(this).width("100%");	
		$(this).children().height("480px");
		$(this).children().width("100%");
		$(this).parent().parent().height("520px");
		$(this).parent().parent().parent().parent().children('.divex').append('<img id="backit" src="arrow.jpg"/>');
		var tfg= $(this);
		$(this).children("p").css({"text-align":"center","font-size":"1em"});

	$('#backit').click(function(){
		$(tfg).height("250px");	
		$(tfg).width("24%");	
		$(tfg).children().height("180px");
		$(tfg).children().width("88%");	
		$(tfg).parent().siblings().fadeIn(200);
		$(tfg).siblings().fadeIn(200);	
		$('#backit').remove();	
		$(tfg).children("p").css({"text-align":"left","font-size":"13px"});
		});		
	});
		$('input[name=nam]').one("click",function(){
			$(this).val("");
		});

	$('textarea[name=com]').one("click",function(){
		$(this).val("");
		$(this).animate({width:"65%"},1000);
		$(this).css('border-color','#25488e');
	});
		var i=20;

	$('button').click(function(){
		var nm=''+$('input[name=nam]').val();
		var cnt=''+$('textarea[name=com]').val();
		var chk=$.trim($('textarea[name=com]').val());
		if(chk.length==0){}
		else{
			i=i+7.6;
		$('#bottom_most').height(i+"em");
		$('#bottom_most').append("<br><img id=cmntmon src='Iron_Fox.png'/>");
		$('#bottom_most').append("<div id='inputnew'><g id='fhg'></g></div>");
		$('g:last').append('<b>'+nm+'</b>'+'<br>'+cnt);
		$('input[name=nam]').val('');
		$('textarea[name=com]').val('');}
	});
});

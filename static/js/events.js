window.addEvent('domready', function (){
    $(document.body).addEvents({
        'click:relay(a[href^=/])': function (event){
            event.stop();
            if (!this.hasClass('disabled')) {
                this.addClass('disabled');

                updateView(this.get('href'));
                if (this.hasClass('startWork')) setCurrent(this); 
                if (this.get('id') == 'StopWork') stopWork(); 
            }
        }
    });

    function updateView(url){
        var url = new URI(url).setData({'format': "bare-html"});
        $('MainContainer').load(url.toString());
    }

    function setCurrent(link){
        $$('.case.current').removeClass('current');
        link.getParent('li').addClass('current');
    }

    function stopWork(){
        $$('.case.current').removeClass('current');
        $('CurrentCase').destroy();
    }
});




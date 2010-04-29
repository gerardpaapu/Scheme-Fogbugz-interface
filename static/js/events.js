window.addEvent('domready', function (){
    $(document.body).addEvents({
        'click:relay(a[href^=/])': function (event){
            event.stop();
            if (!this.hasClass('disabled')) {
                this.addClass('disabled');

                var url = new URI(this.get('href')).setData({'format': "bare-html"});
                $('MainContainer').load(url.toString());
            }
        }
    });
});

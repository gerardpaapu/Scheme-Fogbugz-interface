window.addEvent('domready', function (){
    $(document.body).addEvents({
        'click:relay(a)': function (event){
            event.stop();
            getUpdate(JSONify(this.get('href')));
        }
    });
});

function JSONify(url){

}

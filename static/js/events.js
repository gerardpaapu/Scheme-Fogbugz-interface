window.addEvent('domready', function (){
    $(document.body).addEvents({
        'click:relay(a)': function (event){
            event.stop();
            return console.log('click', this);
        }
    });
});

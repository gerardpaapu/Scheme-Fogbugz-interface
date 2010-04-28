window.addEvent('domready', function (){
    $(document.body).addEvents({
        'click:relay(a[href^=/])': function (event){
            event.stop();
            if (!this.hasClass('disabled')) {
                this.addClass('disabled');
                update(this.get('href'));
            }
        }
    });
});

function update(url){
    new Request.JSON({
        'url': url,
        'data': {'format': "json"},
        'onSuccess': redrawView
    });
}

function redrawView(data){
    var elements = [
        logoutLink(),
        currentCase(),
              
    ];

    new Element('div', {'id': 'MainContainer'})
        .adopt(elements)
        .replaces('MainContainer');
};

function logoutLink(){
    return new Element('a', {'href': "/logout", 'id': "Logout", 'text': "logout"});
}

function currentCase(ccase){
    return Elements.from(
        ('<div id="CurrentCase">'
        +'    <span class="description">' 
        +'        You\'re working on <q>{project}: {title}</q>'
        +'    </span>'
        +'    <a id="StopWork" href="/stop-work/{id}">Stop Work</a>'
        +'    <a id="ResolveCase" href="/resolve-bug/{id}">Resolve</a>'
        +'</div>').substitute(ccase));
}

function estimatedProject(p){
    var resolved = p.filter('resolved'),
        unresolved = p.filter('unresolved');

    return $$(
        new Element('h2', {'text': p.title}),
        new Element('ul').adopt(
            resolved.map(resolvedCase),
            unresolved.map(unresolvedCase)));
}

function unresolvedCase(c){
    return Elements.from(
        ('<li class="case">'
        +'    <a class="details" href="http://fogbugz.phosphor.co.nz/default.asp?{id}">'
        +'        {id}'
        +'    </a>'
        +'    <a class="startWork" href="/start-work/{id}">'
        +'        {title}'
        +'    </a>'
        +'    <p>'
        +'    I just spent'
        +'       (<a class="quick-interval" href="/quick-interval/{id}/5">5</a>,'
        +'        <a class="quick-interval" href="/quick-interval/{id}/10">10</a>,'
        +'        <a class="quick-interval" href="/quick-interval/{id}/15">15</a>,'
        +'        <a class="quick-interval" href="/quick-interval/{id}/30">30</a>) minutes on this'
        +'    </p>'
        +'</li>').substitute(c));
}

function resolvedCase(c){
    return Elements.from(
        ('<li class="case resolved">'
        +'    <a class="details" href="http://fogbugz.phosphor.co.nz/default.asp?{id}">'
        +'        {id}'
        +'    </a>'
        +'    {title}'
        +'    <a href="/close-bug/{id}" class="close-case">close</a>'
        +'</li>').substitute(c));
}



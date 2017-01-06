var App = function($, books){
    var self = {};
    
    self._books = [];
    self._creators = {};

    self.init = function(books){
        self.creators = self.buildCreators(books);
        
        self.hideLoading();
        
        self.renderCreators();
        self.renderBooks();
        
        self.makeCreatorsGoToBottom();
    };
    
    self.buildCreators = function(books){
        var creators = {};

        // Add books to their creator
        
        $.each(books, function(i, book){
            var creator = book._creators[0];

            if (!(creator in creators)){
                creators[creator] = [];
            }

            creators[creator].push(book);
        });
        
        // Sort books within the creators
        
        Object.keys(creators).map(function(key, index){
            creators[key].sort(function(book1, book2) {
                var title1 = book1._titles[0];
                var title2 = book2._titles[0];

                if (title1 > title2) {
                    return 1;
                }
                else if (title1 < title2) {
                    return -1;
                }
                else {
                    return 0;
                }
            });
        });
        
        return creators;
    };
    
    self.hideLoading = function(){
        $('#loading').hide();
    };
    
    self.renderCreators = function(){
        self.iterate_over_dict_sorted(self.creators, function(creator, books) {
            var $creator = $('.prototype-creator').clone();

            $creator.removeClass('prototype-creator').addClass('creator');

            $creator.find('.name').html(creator);
            
            $.each(books, function(i, book){
                var $book = $('.prototype-creator-book').clone();
                
                $book.removeClass('prototype-creator-book').addClass('book');
                $book.find('a').attr('href', book._path).html(book._titles[0]);
                
                $creator.find('.books').append($book)
            });

            $('#creators').append($creator);
        });
    };
    
    self.renderBooks = function(){
        self.iterate_over_dict_sorted(self.creators, function(creator, books){
            $.each(books, function(i, book){
                var $book = $('.prototype-book').clone();

                $book.removeClass('prototype-book').addClass('book');

                var cover = book._maybeCover;

                if (cover){
                    $book.find('.image a').attr('href', book._path);
                    $book.find('img').attr('src', cover._thumbnailPath);
                }

                $book.find('.title').html(book._titles[0]);
                $book.find('.creator').html(book._creators[0]);

                $('#books').append($book);
            });
        });
    };
    
    self.iterate_over_dict_sorted = function(dict, f){
        var keys = Object.keys(dict);
        keys.sort();
        
        $.each(keys, function(i, key){
            f(key, dict[key]);
        });
    };
    
    self.makeCreatorsGoToBottom = function(){
        if ($(window).width() > 650){
            $('#creators').height($(document).height());
        }
    };
    
    self.init(books);
    
    return self;
};

(function($){
    $(function(){
        $.getJSON('app.cgi', function(books){
            App($, books);
        });
    });
})(jQuery);
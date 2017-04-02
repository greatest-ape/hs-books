var Book = function($, title, path, cover, author, languages, textBytes, authorInstance){
    var self = {};

    self.title     = title;
    self.path      = path;
    self.cover     = cover;
    self.author    = author;
    self.languages = languages;
    self.textBytes = textBytes;
    self.authorInstance = authorInstance;

    self.$bookWithImage = null;
    self.$bookInList = null;

    self.language  = null;
    self.lengthIndicator = 0;

    self.init = function(){
        self.lengthIndicator = Math.round(2 * Math.pow(self.textBytes / (1024 * 1), 1/2));
        self.language  = self.getLanguage(self.languages);
    }

    self.render = function(){
        self._renderWithImage();
        self._renderInList();
    }

    self.matchesKeywords = function(keywords){
        return keywords.every(function(keyword){
            var inTitle = self.title.toLowerCase().indexOf(keyword) > -1;
            var isLanguage = self.language ? self.language.toLowerCase() == keyword: false;

            return inTitle || isLanguage;
        })
    }
    
    self.show = function(){
        self.$bookWithImage.show();
        self.$bookInList.show();
    }
    
    self.hide = function(){
        self.$bookWithImage.hide();
        self.$bookInList.hide();
    }

    self.getLanguage = function(languages){
        var lang = null;

        if (languages.length > 0) {
            switch (self.languages[0].substr(0, 2)) {
                case 'en':
                    lang = 'English';
                    break;
                case 'fr':
                    lang = 'French';
                    break;
                case 'de':
                    lang = 'German';
                    break;
                case 'sv':
                    lang = 'Swedish';
                    break;
            }
        }

        return lang;
    }

    self._renderWithImage = function(){
        var $book = $('.prototype-book').clone();

        $book.removeClass('prototype-book').addClass('book');

        if (self.cover){
            $book.find('.image a').attr('href', self.path);
            $book.find('img').attr('src', self.cover._thumbnailPath);
        }

        $book.find('.creator').html(self.author);
        $book.find('.title a').html(self.title).attr("href", self.path);
        $book.find('.title .language').html(self.language);
        $book.find('.title .length-indicator').css('width', self.lengthIndicator + 'px').html('&nbsp;');

        $('#books').append($book);

        self.$bookWithImage = $book;
    }

    self._renderInList = function(){
        var $book = $('.prototype-creator-book').clone();
        
        $book.removeClass('prototype-creator-book').addClass('book');
        $book.find('a').attr('href', self.path).html(self.title);
        $book.find('.length-indicator').css('width', self.lengthIndicator + 'px').html('&nbsp;');
        $book.find('.language').html(self.language);
        
        self.authorInstance.$author.find('.books').append($book);

        self.$bookInList = $book;
    }

    self.init();

    return self;
}


var Author = function($, name, books){
    var self = {};

    self.name = "";
    self.books = [];

    self.$author = null;

    self.init = function(){
        self.name = name;

        self.books = books.map(function(book){
            return Book($, book._titles[0], book._path, book._maybeCover, self.name, book._languages, book._textBytes, self);
        });

        self.render();
    }

    self.showOnMatch = function(keywords){
        var matchingBooks = self.books.filter(function(book){
            return book.matchesKeywords(keywords);
        })

        var authorMatch = keywords.every(function(keyword){
            return self.name.toLowerCase().indexOf(keyword) > -1
        })

        // Start with hiding everything
        self.$author.hide();
        $.each(self.books, function(i, book) { book.hide() });

        // Display all books from this author
        if (authorMatch) {
            self.$author.show();

            $.each(self.books, function(i, book) { book.show() });
        }
        // Display all matching books
        else if (matchingBooks.length > 0) {
            self.$author.show();

            $.each(matchingBooks, function(i, book) { book.show() });
        }
        // Hide author and books
        else {
            self.$author.hide();

            $.each(self.books, function(i, book) { book.hide() });
        }
    }

    self.showAll = function(){
        self.$author.show();

        $.each(self.books, function(i, book) { book.show() });
    }

    self.render = function(){
        self.$author = $('.prototype-creator').clone();

        self.$author.removeClass('prototype-creator').addClass('creator');

        self.$author.find('.name').html(self.name);

        self.$author.appendTo($('#creators'));

        $.each(self.books, function(i, book){
            book.render();
        });
    }

    self.init();

    return self;
}


var App = function($, books){
    var self = {};
    
    self.authors = [];

    self.init = function(books){
        var creators = self._groupBooksByAuthor(books);
        
        $('#loading').hide();

        self._createAuthors(creators);

        $('#filter').show();

        self._makeAuthorsGoToBottom();
    };

    self.search = function(query){
        var keywords = query.trim().toLowerCase().split(" ");

        $.each(self.authors, function(i, author){
            author.showOnMatch(keywords);
        });
    }

    self.showAll = function(){
        $.each(self.authors, function(i, author){
            author.showAll();
        });
    }
    
    self._groupBooksByAuthor = function(books){
        var authors = {};

        // Add books to their author
        
        $.each(books, function(i, book){
            var author = book._creators[0];

            if (!(author in authors)){
                authors[author] = [];
            }

            authors[author].push(book);
        });
        
        // Sort books within the authors
        
        Object.keys(authors).map(function(key, index){
            authors[key].sort(function(book1, book2) {
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
        
        return authors;
    };

    self._createAuthors = function(creators){
        self._iterate_over_dict_sorted(creators, function(creator, books) {
            self.authors.push(Author($, creator, books));
        });
    }
    
    self._iterate_over_dict_sorted = function(dict, f){
        var keys = Object.keys(dict);
        keys.sort();
        
        $.each(keys, function(i, key){
            f(key, dict[key]);
        });
    };
    
    self._makeAuthorsGoToBottom = function(){
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
            var app = App($, books);

            $('#filter input').on('change', function(){
                var query = $(this).val();

                if (query == ''){
                    app.showAll();
                }
                else {
                    app.search(query);
                }
            });
        });
    });
})(jQuery);

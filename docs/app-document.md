The first argument is a `document` function. This turns an initial page fragment into a full document, complete with `<script>` and `<link>` tags.

The `quickStartDocument` adds some required CSS and Javascript, including live reload. But you can customize it however you wish. For example, here's how you might set your own title and add some global css

    #EMBED Example.Document documentHead

    #EMBED Example.Document main

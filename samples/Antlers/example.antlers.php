<!doctype html>
<html lang="{{ site:short_locale }}">
    <head>
        <meta charset="utf-8">
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <title>{{ title ?? site:name }}</title>
        <link rel="stylesheet" href="{{ mix src='css/tailwind.css' }}">
    </head>
    <body class="bg-gray-100 font-sans leading-normal text-grey-800">
        <header class="bg-beige text-brown w-full h-12 flex justify-between px-8">
            <div class="my-auto">
                <span>B. Kierberger</span>
            </div>
            <ul class="list-none flex-row flex w-1/3 justify-between uppercase my-auto font-light">
                <li><a href="ueber-mich" title="Über Mich">Über Mich</a></li>
                <li><a href="qi-gong" title="Qi Gong">Qi Gong</a></li>
                <li><a href="stroemen" title="Strömen">Strömen</a></li>
                <li><a href="kontakt" title="Kontakt">Kontakt</a></li>
                <li><a href="kurse" title="Kurse">Kurse</a></li>
            </ul>
        </header>
        <div class="">
            {{ template_content }}
        </div>
        <footer class="bg-brown text-beige w-full h-12 flex justify-between px-8">
            <div class="my-auto">
                <span>
                © <?php echo date('Y') ?> Barbara Kierberger
                </span>
            </div>
            <ul class="flex flex-row w-1/5 justify-between my-auto text-sm font-light">
                <li><a href="datenschutzrichtlinien">Datenschutzrichtlinien</a>
                <li><a href="impressum">Impressum</a></li>
            </ul>
        </footer>
        <script src="{{ mix src='/js/site.js' }}"></script>
    </body>
</html>

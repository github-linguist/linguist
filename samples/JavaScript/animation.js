<svg xmlns="http://www.w3.org/2000/svg"
     width="100" height="40">
    <script type="text/javascript">
        function animate(element) {
            var textNode = element.childNodes[0]; // assuming no other children
            var text = textNode.data;
            var reverse = false;

            element.onclick = function () { reverse = !reverse; };

            setInterval(function () {
                if (reverse)
                    text = text.substring(1) + text[0];
                else
                    text = text[text.length - 1] + text.substring(0, text.length - 1);
                textNode.data = text;
            }, 100);
        }
    </script>

    <rect width="100" height="40" fill="yellow"/>
    <text x="2" y="20" onload="animate(this);">Hello World! </text>
</svg>

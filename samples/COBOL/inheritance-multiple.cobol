       CLASS-ID. Camera.
           *> ...
       END CLASS Camera.

       CLASS-ID. Mobile-Phone.
           *> ...
       END CLASS Mobile-Phone.

       CLASS-ID. Camera-Phone INHERITS Camera, Mobile-Phone.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           CLASS Camera
           CLASS Mobile-Phone.

           *> ...
       END CLASS Camera-Phone.

use SDL;
use SDL::Events;
use SDLx::App;

my $app = SDLx::App->new;
$app->add_event_handler( sub {
	my $event = shift;
	if( $event->type == SDL_MOUSEMOTION ) {
		printf( "x=%d y=%d\n", $event->motion_x, $event->motion_y );
		$app->stop
	}
} );
$app->run;

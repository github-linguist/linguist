using Uno;
using Uno.Collections;
using Uno.Graphics;
using Uno.Scenes;
using Uno.Designer;
using Uno.Content;
using Uno.Content.Models;
using Uno.UI;

namespace PONG2D
{
	public class PlayerPads : Node
	{

		Image _player1Image;
		Image _player2Image;

		[Inline]
		public Image Player1
		{
			get { return _player1Image; }
			set
			{
				if (_player1Image != value)
				{
					_player1Image = value;
					
				}
			}
		}

		[Inline]
		public Image Player2
		{
			get { return _player2Image; }
			set
			{
				if (_player2Image != value)
				{
					_player2Image = value;
					
				}
			}
		}

		[Hide]
		public float2 Player1Pos
		{
			get { return (Player1.ActualPosition); }
			set
			{
				if (Player1 != null)
					Player1.Position = value;
				
			}
		}

		[Hide]
		public float2 Player2Pos
		{
			get { return (Player2.ActualPosition); }
			set
			{
				if (Player2 != null)
					Player2.Position = value;
				
			}
		}
		
		public Rect Player1Rect
		{
			get { return new Rect(Player1Pos, float2(Player1.Width, Player2.Height)); }
			set
			{
				Player1Pos = value.Position;
				if (Player1 != null)
				{
					Player1.Width = value.Size.X;
					Player1.Height = value.Size.Y;
				}
			}
		}
		
		public Rect Player2Rect
		{
			get { return new Rect(Player2Pos, float2(Player2.Width, Player2.Height)); }
			set
			{
				Player2Pos = value.Position;
				if (Player2 != null)
				{
					Player2.Width = value.Size.X;
					Player2.Height = value.Size.Y;
				}
			}
		}

		public Ball Ball
		{
			get;
			set;
		}
		
		public float PadVelocity { get; set; }

		public PlayerPads()
		{

		}

		void UpdatePositions()
		{
			
		}

		protected override void OnUpdate()
		{
			base.OnUpdate();

			if (Input.IsKeyDown(Uno.Platform.Key.W))
			{
				Player1Pos = float2(0, Player1Pos.Y - PadVelocity);
			}

			if (Input.IsKeyDown(Uno.Platform.Key.S))
			{
				Player1Pos = float2(0, Player1Pos.Y + PadVelocity);
			}

			if (Input.IsKeyDown(Uno.Platform.Key.Up))
			{
				Player2Pos = float2(0, Player2Pos.Y - PadVelocity);
			}

			if (Input.IsKeyDown(Uno.Platform.Key.Down))
			{
				Player2Pos = float2(0, Player2Pos.Y + PadVelocity);
			}
			
			if (Ball != null)
			{
				
				if (Ball.BallRectangle.Intersects(Player1Rect) ||
					Ball.BallRectangle.Intersects(Player2Rect))
				{
					
					Ball.BallVelocity = float2(Ball.BallVelocity.X * -1f, Ball.BallVelocity.Y);
				}
			}
			
		}

	}
}
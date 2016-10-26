using Uno;
using Uno.Collections;
using Uno.Graphics;
using Uno.Scenes;
using Uno.Content;
using Uno.Content.Models;

namespace PONG2D
{
	public class Pong : Node
	{
		float2 _player1Pos;
		float2 _player2Pos;
		float2 ballPosition;
		float2 ballVelocity;
		float2 rectangleSize;
		
		Rect player1Rect;
		Rect player2Rect;
		Rect ballRect;
		
		float2 resolution = Context.VirtualResolution;
		
		Random random = new Random(1);
		
		
		float2 Player1Pos
		{
			get { return _player1Pos; }
			set
			{
				_player1Pos = Math.Clamp(value, float2(0, 0), resolution - rectangleSize);
			}
		}
		
		float2 Player2Pos
		{
			get { return _player2Pos; }
			set
			{
				_player2Pos = Math.Clamp(value, float2(0, 0), resolution - rectangleSize);
			}
		}
		
		public Pong()
		{
			Uno.Scenes.Input.AddGlobalListener(this);
		}
		
		protected override void OnInitialize()
		{
			base.OnInitialize();
			UpdateValues();	
			
		}

		void UpdateValues()
		{
			rectangleSize = float2(resolution.X / 80f, resolution.Y / 5f);
			_player1Pos = float2(0f);
			_player2Pos = float2(Context.VirtualResolution.X - rectangleSize.X, 0f);
			
			player1Rect = new Rect(_player1Pos, rectangleSize);
			player2Rect = new Rect(_player2Pos, rectangleSize);
			
			ballPosition = float2(resolution.X * 0.5f - 10f, resolution.Y * 0.5f - 10f);
			ballRect = new Rect(ballPosition, float2(20f));
			
			
			SpwanBall();
			
		}
		
		void SpwanBall()
		{
			ballRect.Position = float2(resolution.X * 0.5f - 10f, resolution.Y * 0.5f - 10f);
			ballVelocity = float2(5f, 10f) * 0.5f;
		}
		
		void OnWindowResize(object sender, EventArgs args)
		{
			//UpdateValues();
		}
		
		protected override void OnUpdate()
		{
			base.OnUpdate();
			
			var padVelocity = resolution.Y * (float)Application.Current.FrameInterval * 4f;
			if (Input.IsKeyDown(Uno.Platform.Key.Up))
			{
				Player1Pos = float2(Player1Pos.X, Player1Pos.Y - padVelocity);	
			}
			
			if (Input.IsKeyDown(Uno.Platform.Key.Down))
			{
				Player1Pos = float2(Player1Pos.X, Player1Pos.Y + padVelocity);	
			}

			if (Input.IsKeyDown(Uno.Platform.Key.W))
			{
				Player2Pos = float2(Player2Pos.X, Player2Pos.Y - padVelocity);	
			}

			if (Input.IsKeyDown(Uno.Platform.Key.S))
			{
				Player2Pos = float2(Player2Pos.X, Player2Pos.Y + padVelocity);	
			}
			player1Rect.Position = Player1Pos;
			player2Rect.Position = Player2Pos;
			
			if (ballRect.Position.X > resolution.X || ballRect.Position.X < 0)
			{
				SpwanBall();
			}
			if (ballRect.Position.Y > resolution.Y ||
				ballRect.Position.Y < 0)
			{
				ballVelocity.Y *= -1f;
			}
			
			if (ballRect.Intersects(player1Rect) ||
				ballRect.Intersects(player2Rect))
			{
				ballVelocity.X *= -1f;
			}
			
			ballRect.Position += ballVelocity;
			
		}
		
        protected override void OnDraw()
        {
			Uno.Drawing.RoundedRectangle.Draw(player1Rect.Position, player1Rect.Size, float4(1f), 0);
			Uno.Drawing.RoundedRectangle.Draw(player2Rect.Position, player2Rect.Size, float4(1f), 0);
			Uno.Drawing.RoundedRectangle.Draw(ballRect.Position, ballRect.Size, float4(1f), 0f);
        }
	}
}
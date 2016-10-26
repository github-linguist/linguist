using Uno;
using Uno.Collections;
using Uno.Graphics;
using Uno.Scenes;
using Uno.Content;
using Uno.Content.Models;
using Uno.Physics.Box2D;

using TowerBuilder.Box2DMath;

namespace TowerBuilder
{
	public class TowerBlock : TestBed
	{
		Body floorBody, deleteBody, mouseBody;

		private List<Body> bodies = new List<Body>();
		private List<Body> bodiesToDelete = new List<Body>();

		private ContactListener contactListener;

		protected override void OnInitializeTestBed()
		{
			World.Gravity = float2(0, -25.0f);
			World.ContactListener = contactListener = new ContactListener(this);
			
			bodies.Clear();
			bodiesToDelete.Clear();

			CreateFloor();
			CreateDeleteBody();
			CreateBox2();
		}

		void CreateFloor()
		{
			var bodyDef = new BodyDef();
			bodyDef.position = float2(0, -40.0f);

			floorBody = World.CreateBody(bodyDef);

			var shape = new PolygonShape();
			shape.SetAsBox(30.0f, 10.0f);

			var fixtureDef = new FixtureDef();
			fixtureDef.shape = shape;
			fixtureDef.density = 1.0f;

			floorBody.CreateFixture(fixtureDef);
		}

		void CreateDeleteBody()
		{
			var bodyDef = new BodyDef();
			bodyDef.position = float2(0, -44.0f);

			deleteBody = World.CreateBody(bodyDef);

			var shape = new PolygonShape();
			shape.SetAsBox(200.0f, 10.0f);

			var fixtureDef = new FixtureDef();
			fixtureDef.shape = shape;
			fixtureDef.density = 1.0f;

			deleteBody.CreateFixture(fixtureDef);
		}

		Random random = new Random((int) (Uno.Diagnostics.Clock.GetSeconds() * 1000000));
		void CreateBox2()
		{
			var bodyDef = new BodyDef();
			bodyDef.type = BodyType.Dynamic;
			bodyDef.position = float2(random.NextFloat(-25f, 25f), 50.0f);
			bodyDef.angularVelocity = random.NextFloat() * 40 - 20;
			bodyDef.userData = float3(0, 0, 0);

			var body = World.CreateBody(bodyDef);

			var shape = new PolygonShape();
			shape.SetAsBox(0.75f, 0.75f);

			var fixtureDef = new FixtureDef();
			fixtureDef.shape = shape;
			fixtureDef.density = 5.0f;
			//fixtureDef.friction = 0.75f;

			body.CreateFixture(fixtureDef);

			bodies.Add(body);
		}
		
		private int c = 0;
		protected override void OnFixedUpdate()
		{
			base.OnFixedUpdate();
			
			debug_log bodies.Count;
			if(c++ % 8 == 0 && bodies.Count < 20) CreateBox2();

			foreach(var body in bodiesToDelete)
			{
				World.DestroyBody(body);
				bodies.Remove(body);
			}

			bodiesToDelete.Clear();
		}

		public class ContactListener : IContactListener
		{
			private TowerBlock b;
			public ContactListener(TowerBlock b)
			{
				this.b = b;
			}

			public void BeginContact(Contact contact)
			{
				if(contact.GetFixtureA().GetBody() == b.deleteBody)
				{
					b.bodiesToDelete.Add(contact.GetFixtureB().GetBody());
				}
				else if(contact.GetFixtureB().GetBody() == b.deleteBody)
				{
					b.bodiesToDelete.Add(contact.GetFixtureA().GetBody());
				}
			}

        	public void EndContact(Contact contact)  {}
			public void PreSolve(Contact contact, ref Manifold manifold) {}
			public void PostSolve(Contact contact, ref ContactImpulse impulse) {}
		}

	}
}

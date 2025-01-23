/** 
* @file Entity.h
* @page EntityPage Entity
* @brief represent an entity in the game
* @author vinz243
* @version 0.1.0
* This file represents an Entity in the game system
* This parent type is a static entity which is shown and loaded into the Physics engine but never updated
*/

#ifndef ENTITY_H
#define ENTITY_H

#include "base.h"
/// @namespace Whitedrop
namespace Whitedrop {
	/** @class Entity
	* This parent type is a static entity which is shown and loaded into the Physics engine but never updated
	*/
	class Entity {
	public:
		/**
		 * @brief Create static entity
		 * @details creates a static entity instance according to the mesh and the id, the position
		 * This needs to be attached to a World after!
		 * The material name is not the file name but the material name!
		 * @ref WorldPage
		 * @param mesh the name of the mesh for the object, file must be in media/meshes
		 * @param id an unique identifier for the object, shortest as possible
		 * @param dimensions an Ogre::Vector3 which contains the dimensions in meter
		 * @param position the Vector3 which contains it position 
		 * @param material the material name
		 */
		Entity(std::string mesh, std::string id, Ogre::Vector3 dimensions, Ogre::Vector3 position, std::string material);
		/**
		 * @brief The copy constructor
		 * @details A copy constr
		 * 
		 * @param ref the Entity to be copied from
		 */
		Entity(const Entity &ref);

		/**
		 * @brief The assignement operator
		 * @details 
		 * 
		 * @param ent the entity to be copied
		 */
		Entity& operator=(const Entity ent);

		/**
		 * @brief destrctor
		 * @details
		 */
		virtual ~Entity(void);

		/**
		 * @brief a constance type of the entity
		 * @details depends of the class. 
		 * May contain STATIC, DYNAMIC or ETHERAL
		 */
		const std::string type = "STATIC";

		/**
		 * @brief Attach the entity to specified sceneManager
		 * @details This creates the OgreEntity using sceneMgr,
		 * set material, create a Node with name as `<id>_n`,
		 * scale it to match dimensions and translate the node to pos
		 * @param sceneMgr the scene manager to use
		 */
		virtual void setup(Ogre::SceneManager* sceneMgr);

		/**
		 * @brief the update method
		 * @details this method should be called on each world update.
		 * Even though the method is necessary declared, the main impl of 
		 * a static entity should be empty since it is not updated by physics
		 * However, a Dynamic entity should implement this function in order to:
		 * 1) Get from the physics engine the actor position in the physic world
		 * 2) Update the OgreEntity position and rotation from the previous actor
		 * @return whether it was successful or not, if falsey engine should stop
		 */
		virtual bool update(void);

	protected:
		std::string 			mMesh = "cube.mesh";
		std::string 			mId;
		std::string 			mMaterial;
		Ogre::Vector3 			mDimensions;
		Ogre::Vector3 			mPosition;
		Ogre::Entity* 			mEntity;
		Ogre::SceneNode* 		mNode;

	};
}


#endif
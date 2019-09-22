-- | Description: Low-level Haskell bindings to Chipmunk2D physics library
-- Chiphunk is a __low-level__ Haskell bindings for the <https://chipmunk-physics.net/ Chipmunk2D physics engine>.
-- It includes most (almost all) of the functions mentioned in the main documentation for Chipmunk2D,
-- except for some (relatively) exotic ones, which may be added later per request.
module Chiphunk.Low
  ( -- * Disclaymer

    -- | This bindings are so low-level so that they even require you to free the memory the Chipmunk2D has allocated
    -- for your objects. Module with more high-level api can be built around this low-level bingings at some point,
    -- in the meantime, however, you're advised to provide wrapper layer for your games so that you do not have to
    -- keep track of such things in the core of your game's logic.
    --
    -- See below for an adoptation of the original Chipmunk documentation available
    -- <https://chipmunk-physics.net/release/ChipmunkLatest-Docs/ here>. I've skipped some sections not related
    -- to the bindings, like the reason author chose C language in the first place and limitations of the C api.
    -- Obviously, it's thanks to that choice that I was able to write bindings around the library in Haskell.
    --
    -- __Howling Moon Software is not affiliated with this bindings.__
    -- __In all of the following \"I\" refers to the original documentation author for Chipmunk2D.__

    -- * Chipmunk2D 7.0.2

    -- | Chipmunk2D is a 2D rigid body physics library distributed under the MIT license.
    -- It is blazingly fast, portable, numerically stable, and easy to use.
    -- For this reason it has been used in hundreds of games across just about every system you can name.
    -- This includes top quality titles such as Night Sky for the Wii and many #1 sellers on the iPhone App Store!
    -- I’ve put thousands of hours of work over many years to make Chipmunk2D what it is today.
    -- If you find Chipmunk2D has saved you a lot of time, please consider
    -- <https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=6666552 donating>.
    -- You’ll make an indie game developer very happy!

    -- | First of all, I would like to give a Erin Catto a big thank you, as Chipmunk2D’s impulse solver
    -- was directly inspired by his example code way back in 2006.
    -- (Now a full fledged physics engine all its own: <http://www.box2d.org/ Box2D.org>).
    -- His contact persistence idea allows for stable stacks of objects with very few iterations of the solver.
    -- My previous solver produced mushy piles of objects or required a large amount of CPU to operate stably.

    -- ** Support

    -- | The best way to get support is to visit the <http://www.slembcke.net/forums/viewforum.php?f=1 Chipmunk Forums>.
    -- There are plenty of people around using Chipmunk on the just about every platform
    -- I’ve ever heard of. If you are working on a commercial project, Howling Moon Software
    -- (my company) is <http://howlingmoonsoftware.com/contracting.php available for contracting>.
    -- We can help with implementing custom Chipmunk behaviors,
    -- as well as priority bug fixes and performance tuning.

    -- ** Contact

    -- | If you find any bugs in Chipmunk, errors or broken links in this document,
    -- or have a question or comment about Chipmunk you can contact me at
    -- slembcke(at)gmail(dot)com. (email or GTalk)

    -- ** License

    -- | Chipmunk is licensed under the MIT license.
    --
    -- @
    -- Copyright (c) 2007-2015 Scott Lembcke and Howling Moon Software
    --
    -- Permission is hereby granted, free of charge, to any person obtaining a copy
    -- of this software and associated documentation files (the "Software"), to deal
    -- in the Software without restriction, including without limitation the rights
    -- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    -- copies of the Software, and to permit persons to whom the Software is
    -- furnished to do so, subject to the following conditions:
    --
    -- The above copyright notice and this permission notice shall be included in
    -- all copies or substantial portions of the Software.
    --
    -- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    -- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    -- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    -- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    -- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    -- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    -- SOFTWARE.
    -- @
    --
    -- This means that you do not have to buy a license or pay to use Chipmunk in commercial projects. (Though we really appreciate donations)

    -- ** Links

    -- |
    -- * <http://chipmunk2d.net/forum Chipmunk Forums> – The official forum Chipmunk2D forum.
    --
    -- * <http://howlingmoonsoftware.com/ Howling Moon Software> – The software company I co-founded.
    -- (We are available for contract work!)
    --
    -- * <http://chipmunk2d.net/games.php Games> – A small list of games we know that use Chipmunk.

    -- * Hello Chipmunk (World)

    -- | Hello world Chipmunk style. Create a simple simulation where a ball falls onto a static line segment,
    -- then rolls off. Print out the coordinates of the ball.
    --
    -- @
    -- main :: IO ()
    -- main = do
    --   let gravity = 'Vect' 0 (-100)
    --
    --   -- Create an empty space.
    --   space <- 'spaceNew'
    --   'spaceGravity' space $= gravity
    --
    --   -- Add a static line segment shape for the ground.
    --   -- We'll make it slightly tilted so the ball will roll off.
    --   -- We attach it to a static body to tell Chipmunk it shouldn't be movable.
    --   static <- get $ 'spaceStaticBody' space
    --   ground <- 'segmentShapeNew' static (Vect (-20) 5) (Vect 20 (-5)) 0
    --   'shapeFriction' ground $= 1
    --   'spaceAddShape' space ground
    --
    --   -- Now let's make a ball that falls onto the line and rolls off.
    --   -- First we need to make a cpBody to hold the physical properties of the object.
    --   -- These include the mass, position, velocity, angle, etc. of the object.
    --   -- Then we attach collision shapes to the 'Body' to give it a size and shape.
    --
    --   let radius = 5
    --   let mass = 1
    --
    --   -- The moment of inertia is like mass for rotation
    --   -- Use the momentFor* functions to help you approximate it.
    --   let moment = 'momentForCircle' mass 0 radius (Vect 0 0)
    --
    --   -- The spaceAdd* functions return the thing that you are adding.
    --   ballBody <- 'bodyNew' mass moment
    --   'spaceAddBody' space ballBody
    --   'bodyPosition' ballBody $= Vect 0 15
    --
    --   -- Now we create the collision shape for the ball.
    --   -- You can create multiple collision shapes that point to the same body.
    --   -- They will all be attached to the body and move around to follow it.
    --   ballShape <- 'circleShapeNew' ballBody radius (Vect 0 0)
    --   'spaceAddShape' space ballShape
    --   'shapeFriction' ballShape $= 0.7
    --
    --   -- Now that it's all set up, we simulate all the objects in the space by
    --   -- stepping forward through time in small increments called steps.
    --   -- It is *highly* recommended to use a fixed size time step.
    --   let timeStep = 1/60
    --   runFor 2 timeStep $ \time -> do
    --     pos <- get $ 'bodyPosition' ballBody
    --     vel <- get $ 'bodyVelocity' ballBody
    --     printf "Time is %4.2f. ballBody is at (%6.2f, %6.2f), it's velocity is (%6.2f, %6.2f).\n"
    --            time (vX pos) (vY pos) (vX vel) (vY vel)
    --
    --     'spaceStep' space timeStep
    --
    --   'shapeFree' ballShape
    --   'bodyFree' ballBody
    --   'shapeFree' ground
    --   'spaceFree' space
    --   where
    --     runFor time step inner = go time
    --       where
    --         go time'
    --           | time' <= 0 = pure ()
    --           | otherwise  = inner (time - time') *> go (time' - step)
    -- @

    -- * Chipmunk2D Basics

    -- ** Overview

    -- | There are 4 basic object types you will use in Chipmunk.

    -- |
    -- * __Rigid Bodies__ ('Body'): A rigid body holds the physical properties of an object.
    -- (mass, position, rotation, velocity, etc.) It does not have a shape until you attach one or more collision shapes
    -- to it. If you’ve done physics with particles before, rigid bodies differ in that they are able to rotate.
    -- Rigid bodies generally tend to have a 1:1 correlation to sprites in a game.
    -- You should structure your game so that you use the position and rotation of the rigid body
    -- for drawing your sprite.
    --
    -- * __Collision Shapes__ ('Shape'): By attaching shapes to bodies, you can define the a body’s shape.
    -- You can attach as many shapes to a single body as you need to in order to define a complex shape.
    -- Shapes contain the surface properties of an object such as how much friction or elasticity it has.
    --
    -- * __Constraints/Joints__ ('Constraint'): Constraints and joints describe how bodies are attached to each other.
    --
    -- * __Spaces__ ('Space'): Spaces are containers for simulating objects in Chipmunk.
    -- You add bodies, shapes and joints to a space and then update the space as a whole.
    -- They control how all the rigid bodies, shapes, and constraints interact together.
    --
    -- There is often confusion between rigid bodies and their collision shapes in Chipmunk
    -- and how they relate to sprites. A sprite would be a visual representation of an object,
    -- while a collision shape is an invisible property that defines how objects should collide.
    -- Both the sprite’s and the collision shape’s position and rotation are controlled by the motion of a rigid body.
    -- Generally you want to create a game object type that ties these things all together.

    -- ** Memory Management the Chipmunk way

    -- | For most of the structures you will use, Chipmunk uses a more or less standard and straightforward set
    -- of memory management functions. Take the 'Space' struct for example:
    --
    -- * 'spaceNew' — Allocates and initializes a 'Space' struct.
    --
    -- * 'spaceFree' — Destroys and frees the 'Space' struct.
    --
    -- You are responsible for freeing any structs that you allocate. Chipmunk does not do reference counting or garbage collection. If you call a new function, you must call the matching free function or you will leak memory.

    -- ** Math the Chipmunk way

    -- | First of all, Chipmunk uses double precision floating point numbers throughout its calculations by default.
    -- This is likely to be faster on most modern desktop processors,
    -- and means you don’t have to worry as much about floating point accuracy.
    --
    -- However, there are a few unique functions you will probably find very useful:
    fClamp
  , fLerp
  , fLerpConst

    -- * Chipmunk Vectors

    -- ** Struct Definition, Constants and Constructors
  , Vect (..)
  , vZero
  , cpv

  -- ** Operations

  -- | (__Note for bindings__: Most of these are Chipmunk2D-style aliases for 'Vect' typeclasses methods:
  -- 'Eq', 'Data.VectorSpace.AdditiveGroup', 'Data.VectorSpace.VectorSpace', 'Data.VectorSpace.InnerSpace',
  -- 'Data.Cross.HasCross2')
  , vEql
  , vAdd
  , vSub
  , vNeg
  , vMult
  , vDot
  , vCross
  , vPerp
  , vRPerp
  , vProject
  , vRotate
  , vUnRotate
  , vLength
  , vLengthSq
  , vLerp
  , vLerpConst
  , vSLerp
  , vSLerpConst
  , vNormalize
  , vClamp
  , vDist
  , vDistSq
  , vNear
  , vForAngle
  , vToAngle

    -- * Chipmunk Axis Aligned Bounding Boxes

    -- ** Struct Definition and Constructors

  , BB (..)
  , bbNew
  , bbNewForExtents
  , bbNewForCircle

    -- ** Operations

  , bbIntersects
  , bbContainsBB
  , bbContainsVect
  , bbMerge
  , bbExpand
  , bbCenter
  , bbArea
  , bbMergedArea
  , bbSegmentQuery
  , bbIntersectsSegment
  , bbClampVect
  , bbWrapVect

    -- * Chipmunk Rigid Bodies
  , Body

    -- ** Dynamic, Kinematic, and Static Bodies
  , BodyType (..)

    -- ** Movement, Teleportation, and Velocity

    -- | A graphics engine only needs to know the position of an object for each frame that its drawn.
    -- For a physics engine, this isn’t enough information to calculate a collision response.
    -- When you set the position of a body, you are effectively asking it to teleport itself.
    -- This means that it will instantly move to its new position instead of moving through space and time
    -- like a normal object. If you teleport an object so that it overlaps another one,
    -- the best the physics engine can do is to attempt to push the objects apart again
    -- since there is no information about their movement. This generally results in very mushy looking collisions.
    -- So instead of setting the position of an object, it’s better to set its velocity and allow the physics engine
    -- to update the position. That way it can resolve any resulting colisions natuarally since it knows
    -- how the objects were moving. This is why kinematic bodies work the way they do.
    -- You set the velocity, and the physics updates their position so the two are never out of sync.
    --
    -- For dynamic bodies, setting the velocity explicitly every frame can cause problems.
    -- For example, a problem occurs when a light dynamic body (like a person) is pressed against a heavy dynamic body
    -- (like a car), and you set velocity of the small object so that it’s pushing it into the big body.
    -- To the physics engine, the change in velocity is the same as applying a large impulse
    -- (a very short, very large force). Even if the velocity is low, the large force can allow the small body
    -- to push the big body, even when it normally wouldn’t be able to. For example, a person walking into a car
    -- can overpower the car’s friction and cause it to creep along the ground slowly.
    -- Additionally, when you set the velocity of an object that is already in contact,
    -- it can cause the two objects to overlap by a small amount. The easiest way to avoid both of these problems
    -- is to make smaller changes to the body’s velocity, accelerating it over a fraction of a second
    -- instead of a single frame. An even better solution, which is covered more thoroughly later,
    -- is to use constraints to move the object.

    -- ** Memory Management Functions

    -- | Standard set of Chipmunk memory management functions.
  , bodyNew
  , bodyNewKinematic
  , bodyNewStatic
  , bodyFree

    -- ** Creating Dynamic Bodies

    -- | There are two ways to set up a dynamic body. The easiest option is to create a body
    -- with a mass and moment of 0, and set the mass or density of each collision shape added to the body.
    -- Chipmunk will automatically calculate the mass, moment of inertia, and center of gravity for you.
    -- This is probably preferred in most cases.
    --
    -- The other option is to set the mass of the body when it’s created, and leave the mass of the shapes
    -- added to it as 0.0. This approach is more flexible, but is not as easy to use.
    -- __Don’t__ set the mass of both the body and the shapes. If you do so,
    -- it will recalculate and overwite your custom mass value when the shapes are added to the body.

    -- ** Properties

    -- | Chipmunk provides getter/setter functions for a number of properties on rigid bodies.
    -- Setting most properties automatically wakes the rigid bodies up if they were sleeping.

  , bodyType
  , bodyMass
  , bodyMoment
  , bodyPosition
  , bodyCenterOfGravity
  , bodyVelocity
  , bodyForce
  , bodyAngle
  , bodyAngularVelocity
  , bodyTorque
  , bodyRotation
  , bodySpace
  , bodyUserData

    -- ** Moment of Inertia and Area Helper Functions

    -- | Use the following functions to approximate the moment of inertia for your body,
    -- adding the results together if you want to use more than one.
  , momentForCircle
  , momentForSegment
  , momentForPoly
  , momentForBox

    -- | Use the following functions to get the area for common Chipmunk shapes if you want to approximate masses
    -- or density or whatnot.
  , areaForCircle
  , areaForSegment
  , areaForPoly

    -- ** Coordinate Conversion Functions

    -- | Many things are defined in coordinates local to a body meaning that the (0,0) is at the center of gravity
    -- of the body and the axis rotate along with the body.
  , bodyLocalToWorld
  , bodyWorldToLocal

    -- ** Velocity Conversion Functions

    -- | It’s often useful to know the absolute velocity of a point on the surface of a body
    -- since the angular velocity affects everything except the center of gravity.
  , bodyVelocityAtWorldPoint

    -- ** Applying Forces and Torques

    -- | People are sometimes confused by the difference between a force and an impulse.
    -- An impulse is a very large force applied over a very short period of time.
    -- Some examples are a ball hitting a wall or cannon firing. Chipmunk treats impulses as if they occur
    -- instantaneously by adding directly to the velocity of an object.
    -- Both impulses and forces are affected the mass of an object.
    -- Doubling the mass of the object will halve the effect.
  , bodyApplyForceAtWorldPoint
  , bodyApplyForceAtLocalPoint
  , bodyApplyImpulseAtWorldPoint
  , bodyApplyImpulseAtLocalPoint

    -- ** Sleeping Functions

    -- | Chipmunk supports a sleeping feature which improves performance by not simulating groups of objects
    -- that aren’t moving. Read more about it in the 'Space' section.
  , bodyIsSleeping
  , bodyActivate
  , bodySleep
  , bodyActivateStatic
  , bodySleepWithGroup

    -- ** Iterators
  , BodyShapeIteratorFunc
  , bodyEachShape
  , BodyConstraintIteratorFunc
  , bodyEachConstraint
  , BodyArbiterIteratorFunc
  , bodyEachArbiter

    -- * Chipmunk Collision Shapes

  , Shape

    -- ** Properties

    -- | Chipmunk provides getter/setter functions for a number of properties on collision shapes.
    -- Setting most properties will automatically wake the attached rigid body, if it’s sleeping.

  , shapeBody
  , shapeBB
  , shapeSensor
  , shapeElasticity
  , shapeFriction
  , shapeSurfaceVelocity
  , shapeCollisionType
  , shapeMass
  , shapeDensity
  , ShapeFilter (..)
  , shapeFilter
  , shapeSpace
  , shapeUserData

    -- ** Fast Collision Filtering using ShapeFilter

    -- | Chipmunk has two primary means of ignoring collisions: groups and category masks.
    --
    -- __Groups__ are used to ignore collisions between parts on a complex object.
    -- A ragdoll is a good example. When jointing an arm onto the torso, you’ll want them to allow them to overlap.
    -- Groups allow you to do exactly that. Shapes that have the same group don’t generate collisions.
    -- So by placing all of the shapes in a ragdoll in the same group, you’ll prevent it from colliding
    -- against other parts of itself.
    -- __Category__ masks allow you to mark which categories an object belongs to
    -- and which categories it collidies with.
    --
    -- For example, a game has four collision categories: player (0), enemy (1), player bullet (2),
    -- and enemy bullet (3). Neither players nor enemies should not collide with their own bullets,
    -- and bullets should not collide with other bullets.
    -- However, players collide with enemy bullets, and enemies collide with player bullets.
    --
    -- +-----------------+-----------------+---------------+
    -- | Object          | Object Category | Category Mask |
    -- +=================+=================+===============+
    -- | \"Player\"      | 1               | 4, 5          |
    -- +-----------------+-----------------+---------------+
    -- | \"Enemy\"       | 2               | 2, 3, 5       |
    -- +-----------------+-----------------+---------------+
    -- | "Player Bullet" | 3               | 1, 5          |
    -- +-----------------+-----------------+---------------+
    -- | "Enemy Bullet"  | 4               | 2, 5          |
    -- +-----------------+-----------------+---------------+
    -- | \"Walls\"       | 5               | 1, 2, 3, 4    |
    -- +-----------------+-----------------+---------------+
    --
    -- Note that everything in this example collides with walls. Additionally, the enemies collide with eachother.
    --
    -- By default, objects exist in every category and collide with every category.
    --
    -- Objects can fall into multiple categories. For instance, you might have a category for a red team,
    -- and have a red player bullet. In the above example, each object only has one category.
    --
    -- There is one last way of filtering collisions using collision handlers. See the section on callbacks
    -- for more information. Collision handlers can be more flexible, but can be slower.
    -- Fast collision filtering rejects collisions before running the expensive collision detection code,
    -- so using groups or category masks is preferred.

    -- ** Memory Management Functions
  , shapeFree

    -- ** Misc functions
  , shapeCacheBB
  , shapeUpdate

    -- ** Working With Circle Shapes
  , circleShapeNew

    -- ** Working With Segment Shapes
  , segmentShapeNew
  , segmentShapeNeighbors

    -- ** Working With Polygon Shapes
  , polyShapeNew
  , polyShapeNewRaw

    -- *** Boxes

    -- | Because boxes are so common in physics games, Chipmunk provides shortcuts to create box shaped polygons.
    -- The boxes will always be centered at the center of gravity of the body you are attaching them to.
    -- Adding a small radius will bevel the corners and can significantly reduce problems
    -- where the box gets stuck on seams in your geometry. If you want to create an off-center box,
    -- you will need to use 'polyShapeNew'.
  , boxShapeNew
  , boxShapeNew2

    -- *** Poly Shape Helper Functions
  , centroidForPoly

    -- *** Convex Hull Helper Functions
  , convexHull
  , convexDecomposition

    -- ** Modifying 'Shape's

    -- | The short answer is that you can’t because the changes would be only picked up as a change to the position
    -- of the shape’s surface, but not its velocity.

    -- ** Notes

    -- |
    -- * You can attach multiple collision shapes to a rigid body. This should allow you to create almost any shape
    -- you could possibly need.
    --
    -- * Shapes attached to the same rigid body will never generate collisions. You don’t have to worry
    -- about overlap when attaching multiple shapes to a rigid body.
    --
    -- * Make sure you add both the body and its collision shapes to a space.

    -- * Chipmunk Spaces
  , Space

    -- ** What Are Iterations, and Why Should I Care?

    -- | Chipmunk uses an iterative solver to figure out the forces between objects in the space.
    -- What this means is that it builds a big list of all of the collisions, joints, and other constraints
    -- between the bodies and makes several passes over the list considering each one individually.
    -- The number of passes it makes is the iteration count, and each iteration makes the solution more accurate.
    -- If you use too many iterations, the physics should look nice and solid, but may use up too much CPU time.
    -- If you use too few iterations, the simulation may seem mushy or bouncy when the objects should be solid.
    -- Setting the number of iterations lets you balance between CPU usage and the accuracy of the physics.
    -- Chipmunk’s default of 10 iterations is sufficient for most simple games.

    -- ** Sleeping

    -- | Spaces can disable entire groups of objects that have stopped moving to save CPU time and battery life.
    -- In order to use this feature you must do two things. You must enable sleeping explicitly
    -- by choosing a time threshold value with 'spaceSetSleepTimeThreshold'. This threshold is the amount of time
    -- something must be idle before it falls asleep. 'spaceSetIdleSpeedThreshold' defines what is considered idle.
    -- If you do not set idle speed threshold explicitly, a value will be chosen automatically
    -- based on the current amount of gravity. Be mindful that objects cannot fall asleep if they are touching
    -- or jointed to a kinematic body.

    -- ** Properties

  , spaceIterations
  , spaceGravity
  , spaceDamping
  , spaceIdleSpeedThreshold
  , spaceSleepTimeThreshold
  , spaceCollisionSlop
  , spaceCollisionBias
  , spaceCollisionPersistence
  , spaceCurrentTimeStep
  , spaceIsLocked
  , spaceUserData
  , spaceStaticBody

    -- ** Memory Management Functions

    -- | More standard Chipmunk memory functions.
  , spaceNew
  , spaceFree

    -- ** Operations

    -- | These functions add and remove shapes, bodies and constraints from space. The add/remove functions
    -- cannot be called from within a callback other than a 'postStep' callback (which is different than a 'postSolve'
    -- callback!). Attempting to add or remove objects from the space while 'spaceStep' is still executing
    -- will throw an assertion. See the callbacks section for more information. Be careful not to free bodies
    -- before removing shapes and constraints attached to them or you will cause crashes..
    -- The contains functions allow you to check if an object has been added to the space or not.
  , spaceAddShape
  , spaceAddBody
  , spaceAddConstraint
  , spaceRemoveShape
  , spaceRemoveBody
  , spaceRemoveConstraint
  , spaceContainsShape
  , spaceContainsBody
  , spaceContainsConstraint

    -- ** Spatial Indexing

    -- | Occasionally, you might want to update the collision detection data for a shape.
    -- If you move a static shape or a static body you must do this to let Chipmunk know
    -- it needs to have its collision detection data updated. You may also want to manually update the collision data
    -- for normal shapes if you move them and still want to perform queries against them
    -- before the next call to 'spaceStep'.
  , spaceReindexShape
  , spaceReindexShapesForBody
  , spaceReindexStatic

    -- ** Iterators
  , SpaceBodyIteratorFunc
  , spaceEachBody
  , SpaceShapeIteratorFunc
  , spaceEachShape
  , SpaceConstraintIteratorFunc
  , spaceEachConstraint

    -- ** Simulating the Space
  , spaceStep

    -- * Notes

    -- |
    -- * When removing objects from the space, make sure you remove any other objects that reference it.
    -- For instance, when you remove a body, remove the joints and shapes attached to it.
    --
    -- * Using more iterations or smaller time steps will increase the physics quality, but also increase the CPU usage.

    -- * Chipmunk Constraints
  , Constraint

    -- ** What constraints are and what they are not

    -- | Constraints in Chipmunk are all velocity based constraints.
    -- This means that they act primarily by synchronizing the velocity of two bodies.
    -- A pivot joint holds two anchor points on two separate bodies together by defining equations that say
    -- that the velocity of the anchor points must be the same and calculating impulses to apply to the bodies
    -- to try and keep it that way. A constraint takes a velocity as it’s primary input and produces a velocity change
    -- as its output. Some constraints, (joints in particular) apply velocity changes to correct differences
    -- in positions. More about this in the next section.
    --
    -- A spring connected between two bodies is not a constraint. It’s very constraint-like as it creates forces
    -- that affect the velocities of the two bodies, but a spring takes distances as input and produces forces
    -- as its output. If a spring is not a constraint, then why do I have two varieties of spring constraints you ask?
    -- The reason is because they are damped springs. The damping associated with the spring is a true constraint
    -- that creates velocity changes based on the relative velocities of the two bodies it links.
    -- As it is convenient to put a damper and a spring together most of the time, I figured I might as well just apply
    -- the spring force as part of the constraint instead of having a damper constraint and having the user
    -- calculate and apply their own spring forces separately.

    -- ** Properties

  , constraintBodyA
  , constraintBodyB
  , constraintMaxForce
  , constraintErrorBias
  , constraintMaxBias
  , constraintSpace
  , constraintCollideBodies
  , constraintUserData
  , constraintImpulse

    -- ** Error correction by Feedback

    -- | Joints in Chipmunk are not perfect. A pin joint can’t maintain the exact correct distance between its anchor
    -- points, nor can a pivot joint hold its anchor points completely together. Instead, they are designed
    -- to deal with this by correcting themselves over time. Since Chipmunk 5, you have a fair amount of extra control
    -- over how joints correct themselves and can even use this ability to create physical effects
    -- that allow you to use joints in unique ways:
    --
    -- * Servo motors – Ex: open/close doors or rotate things without going over a maximum force.
    --
    -- * Winches – Pull one object towards another at a constant speed without going over a maximum force.
    --
    -- * Mouse manipulation – Interact with objects smoothly given coarse/shaky mouse input.
    --
    -- There are three properties of 'Constraint' structs that control the error correction,
    -- maxForce, maxBias, and biasCoef. maxForce is pretty self explanatory, a joint or constraint
    -- will not be able to use more than this amount of force in order to function. If it needs more force
    -- to be able to hold itself together, it will fall apart. maxBias is the maximum speed at which error correction
    -- can be applied. If you change a property on a joint so that the joint will have to correct itself,
    -- it normally does so very quickly. By setting a maxSpeed you can make the joint work like a servo,
    -- correcting itself at a constant rate over a longer period of time. Lastly, biasCoef is the percentage
    -- of error corrected every step before clamping to a maximum speed. You can use this
    -- to make joints correct themselves smoothly instead of at a constant speed, but is probably the least useful
    -- of the three properties by far.

    -- ** Constraints and Collision Shapes

    -- | Neither constraints or collision shapes have any knowledge of the other.
    -- When connecting joints to a body the anchor points don’t need to be inside of any shapes attached to the body
    -- and it often makes sense that they shouldn’t. Also, adding a constraint between two bodies
    -- doesn’t prevent their collision shapes from colliding. In fact, this is the primary reason
    -- that the collision group property exists.

    -- ** Video Tour of Current Joint Types

    -- | http://www.youtube.com/watch?v=ZgJJZTS0aMM

    -- ** Shared Memory Management Functions
  , constraintFree

    -- ** Constraint Types

    -- *** Pin Joints
  , pinJointNew

    -- **** Properties
  , pinJointAnchorA
  , pinJointAnchorB
  , pinJointDist

    -- *** Slide Joints
  , slideJointNew

    -- **** Properties
  , slideJointAnchorA
  , slideJointAnchorB
  , slideJointMin
  , slideJointMax

    -- *** Pivot Joints

    -- | (__Note for bindings__: So each instance of pivot joint can be replaced with pin joint with dist of 0?)

  , pivotJointNew
  , pivotJointNew2

    -- **** Properties
  , pivotJointAnchorA
  , pivotJointAnchorB

    -- *** Groove Joint
  , grooveJointNew

    -- **** Properties
  , grooveJointGrooveA
  , grooveJointGrooveB
  , grooveJointAnchorB

    -- *** Damped Spring
  , dampedSpringNew

    -- **** Properties
  , dampedSpringAnchorA
  , dampedSpringAnchorB
  , dampedSpringRestLength
  , dampedSpringStiffness
  , dampedSpringDamping

    -- *** Damped Rotary Spring

    -- | Like a damped spring, but works in an angular fashion.
  , dampedRotarySpringNew

    -- **** Properties
  , dampedRotarySpringRestAngle
  , dampedRotarySpringStiffness
  , dampedRotarySpringDamping

    -- *** Rotary Limit Joint

    -- | Constrains the relative rotations of two bodies.
    -- It is implemented so that it’s possible to for the range to be greater than a full revolution.
  , rotaryLimitJointNew

    -- **** Properties
  , rotaryLimitJointMin
  , rotaryLimitJointMax

    -- *** Ratchet Joint

    -- | Works like a socket wrench.
  , ratchetJointNew

    -- **** Properties
  , ratchetJointAngle
  , ratchetJointPhase
  , ratchetJointRatchet

    -- *** Gear Joint

    -- | Keeps the angular velocity ratio of a pair of bodies constant.
  , gearJointNew

    -- **** Properties
  , gearJointPhase
  , gearJointRatio

    -- *** Simple Motor

    -- | Keeps the relative angular velocity of a pair of bodies constant.
    -- You will usually want to set an force (torque) maximum for motors as otherwise
    -- they will be able to apply a nearly infinite torque to keep the bodies moving.
  , simpleMotorNew

    -- **** Properties
  , simpleMotorRate

    -- ** Notes

    -- | * You can add multiple joints between two bodies, but make sure that they don’t fight.
    -- Doing so can cause the bodies jitter or spin violently.

    -- * Overview of Collision Detection in Chipmunk

    -- | In order to make collision detection in Chipmunk as fast as possible, the process is broken down
    -- into several stages. While I’ve tried to keep it conceptually simple, the implementation can be a bit daunting.
    -- Fortunately as a user of the library, you don’t need to understand everything about how it works.
    -- Though if you are trying to squeeze every bit of performance out of Chipmunk, understanding this section
    -- can be helpful.

    -- ** Spatial Indexing

    -- | A for loop that checks every object against every other object in the scene would be very slow.
    -- So the first stage of the collision detection, commonly called the broadphase, uses a high level
    -- spatial algorithm to decide which pairs of objects to check for collisions.
    -- Currently Chipmunk supports two spatial indexes, an axis-aligned bounding box tree and a spatial hash.
    -- These spatial indexes are able to quickly identify which pairs of shapes are near each other
    -- and should be checked for a collision.

    -- ** Fast Collision Filtering

    -- | After the spatial index figures out which pairs of shapes are likely to be near each other,
    -- it passes each pair back to the space using a callback to perform some additional filtering on the pairs.
    -- Before doing anything else, Chipmunk performs a few quick tests to check if shapes should collide.
    --
    -- * __Bounding Box Test__: The shapes are not colliding if their bounding boxes are not overlapping.
    -- Objects like diagonal line segments can trigger a lot of false positives here,
    -- but it’s unlikely to be something you should worry about.
    --
    -- * __Category Mask Test__: The categories of each shape are bitwise ANDed against the category mask
    -- of the other shape. If either result is 0, the shapes do not collide.
    --
    -- * __Group Test__: Shapes shouldn’t collide with other shapes in the same non-zero group.

    -- ** Constraint Based Filtering

    -- | After fast collision filtering, Chipmunk checks the list of joints on one of the bodies
    -- to see if it has a constraint that attaches it to the other body. If that constraint’s collideBodies
    -- property is false, the collision will be ignored. This check is often very fast
    -- since most scenes don’t contain a lot of constraints.

    -- ** Primitive Shape to Shape Collision Detection

    -- | The most expensive test is to actually check for overlap based on their geometry.
    -- Circle to circle and circle to line collisions are very fast. Segment to segment and poly to poly collisions
    -- are handled using the GJK/EPA algorithms, and get more expensive as the number of vertexes increases.
    -- Simpler shapes make for faster collisions, and often more important, fewer collision points
    -- for the solver to run. Chipmunk uses a small dispatch table to figure out which function to use to check
    -- if the shapes overlap.
    --
    -- Without going into too much detail, the GJK algorithm checks the distance between two objects,
    -- and the EPA algorithm checks how much they are overlapping. If you give you segment and poly shapes
    -- a small radius when creating them, the EPA algorithm can usually be skipped, speeding up the collision detection
    -- considerably. The radius should be at least as big as the amount of allowed collision slop.

    -- ** Collision Handler Filtering

    -- | After checking if two shapes overlap Chipmunk will look to see if you have defined a collision handler
    -- for the collision types of the shapes. This is vital to process collisions events for the gameplay,
    -- but also gives you a very flexible way to filter out collisions. The return value of the begin and preSolve
    -- callbacks determines whether or not the colliding pair of shapes is discarded or not.
    -- Returning true will keep the pair, false will discard it. Rejecting a collision from a begin callback
    -- is permanent, rejecting it from the preSolve only applies to the step it occured in. If you don’t define
    -- a handler for the given collision types, Chipmunk will call the space’s default handler, which by default
    -- is defined to simply accept all collisions.
    --
    -- Wildcard collisions can also return a value, but they are handled in a more complicated way.
    -- When you create a collision handler between two specific collision types, it’s your responsibility
    -- to decide when to call the wildcard handlers and what to do with their return values.
    -- Otherwise, the default is to call the wildcard handler for the first type, then the second type,
    -- and use a logical AND of their return values as filtering value. See DefaultBegin() in cpSpace.c
    -- for more information.
    --
    -- While using callbacks to filter collisions is the most flexible way, keep in mind that by the time your callback
    -- is called all of the most expensive collision detection has already been done. For simulations
    -- with a lot of colliding objects each frame, the time spent finding collisions is small compared to the time
    -- spent solving the physics for them so it may not be a big deal. Fast collision filtering should be preferred
    -- if possible.

    -- * Collision Callbacks

    -- | A physics library without any events or feedback would not be very useful for games.
    -- How would you know when the player bumped into an enemy so that you could take some health points away?
    -- How would you know how hard the car hit something so you don’t play a loud crash noise when a pebble hits it?
    -- What if you need to decide if a collision should be ignored based on specific conditions,
    -- like implementing one way platforms? Chipmunk has a number of powerful callback systems
    -- that you can use to solve these problems.

    -- ** Collision Handlers

    -- | Collision handler function types. While all of them take an arbiter, space, and a user data pointer,
    -- only the begin and preSolve callbacks return a value. See above for more information.
  , CollisionCallback
    -- Collision callbacks are closely associated with 'Arbiter' structs.
    -- You should familiarize yourself with those as well.
    --
    -- __Note__: Shapes tagged as sensors (cpShape.sensor == true) never generate collisions that get processed,
    -- so collisions between sensors shapes and other shapes will never call the postSolve callback.
    -- They still generate begin, and separate callbacks, and the preSolve callback is also called
    -- every frame even though there is no collision response.
    --
    -- __Note #2__: preSolve callbacks are called before the sleeping algorithm runs.
    -- If an object falls asleep, its postSolve callback won’t be called until it’s reawoken.

  , CollisionType
  , CollisionHandler (..)
  , CollisionHandlerPtr
  , spaceAddCollisionHandler
  , spaceAddWildcardHandler
  , spaceAddDefaultCollisionHandler
  , modifyCollisionHandler
  , mkCallback
  , mkCallbackB

    -- ** Post-Step Callbacks

    -- | Post-step callbacks are the one place where you can break the rules about adding or removing objects
    -- from within a callback. In fact, their primary function is to help you safely remove objects from the space
    -- that you wanted to disable or destroy in a collision or query callback.
    --
    -- Post step callbacks are registered as a function and a pointer that is used as a key.
    -- You can only register one postStep callback per key. This prevents you from accidentally removing
    -- an object more than once. For instance, say that you get a collision callback between a bullet and object A.
    -- You want to destroy both the bullet and object A, so you register a postStep callback
    -- to safely remove them from your game. Then you get a second collision callback between the bullet and object B.
    -- You register a postStep callback to remove object B, and a second postStep callback to remove the bullet.
    -- Because you can only register one callback per key, the postStep callback for the bullet
    -- will only be called once and you can’t accidentally try to remove it twice.
  , PostStepFunc
  , spaceAddPostStepCallback

    -- ** Examples

    -- | See
    -- <https://chipmunk-physics.net/release/ChipmunkLatest-Docs/examples.html#CollisionCallbacks the callback examples>
    -- for more information.

    -- * Chipmunk Collision Pairs
  , Arbiter

    -- ** Memory Management

    -- | You will never need to create or free an arbiter. More importantly,
    -- because they are entirely managed by the space you should never store a reference to an arbiter
    -- as you don’t know when they will be freed or reused. Use them within the callback where they are given to you
    -- and then forget about them or copy out the information you need.

    -- ** Properties

  , arbiterRestitution
  , arbiterFriction
  , arbiterSurfaceVelocity
  , arbiterUserData

    -- *** Collision Point(s)
  , arbiterCount
  , arbiterNormal
  , arbiterPointA
  , arbiterPointB
  , arbiterDepth

    -- *** Other
  , arbiterIsFirstContact
  , arbiterIsRemoval

    -- *** Bodies and shapes
  , arbiterShapes
  , arbiterBodies

    -- *** Running wildcard handlers

    -- | These functions invoke the wildcard handlers for a given collision. For custom collision handlers
    -- between specific types or overriding the default handler, you must decide how to invoke the wildcard handlers
    -- since it may be important to call the wildcards first, last, or possibly skip them entirely.
    -- For the begin and preSolve callbacks, you also need to decide what to do with their return values
    -- since they may not agree with each other or the specific handler they were called from.
    -- Every collision handler is defined for two types, the “A” variants of these functions call the wildcard handler
    -- for the first type, and the “B” variants call the handler for the second type.

  , arbiterCallWildcardBeginA
  , arbiterCallWildcardBeginB
  , arbiterCallWildcardPreSolveA
  , arbiterCallWildcardPreSolveB
  , arbiterCallWildcardPostSolveA
  , arbiterCallWildcardPostSolveB
  , arbiterCallWildcardSeparateA
  , arbiterCallWildcardSeparateB

    -- * Misc
  , DataPtr
  , Transform (..)

    -- * Re-exports
  , nullPtr
  , HasGetter (..)
  , HasSetter (..)
  ) where

import Foreign
import Data.StateVar

import Chiphunk.Low.Types
import Chiphunk.Low.Math
import Chiphunk.Low.Helper
import Chiphunk.Low.Vect
import Chiphunk.Low.BB
import Chiphunk.Low.Body
import Chiphunk.Low.Shape
import Chiphunk.Low.Space
import Chiphunk.Low.Constraint
import Chiphunk.Low.Callback
import Chiphunk.Low.Arbiter

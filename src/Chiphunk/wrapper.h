#include <chipmunk/chipmunk.h>

void w_cpvslerp(const cpVect v1, const cpVect v2, const cpFloat t, cpVect *out);

void w_cpvslerpconst(const cpVect v1, const cpVect v2, const cpFloat a, cpVect *out);

void w_cpBodyGetPosition(const cpBody *body, cpVect *out);

void w_cpBodyGetCenterOfGravity(const cpBody *body, cpVect *out);

void w_cpBodyGetVelocity(const cpBody *body, cpVect *out);

void w_cpBodyGetForce(const cpBody *body, cpVect *out);

void w_cpBodyGetRotation(const cpBody *body, cpVect *out);

void w_cpBodyLocalToWorld(const cpBody *body, const cpVect point, cpVect *out);

void w_cpBodyWorldToLocal(const cpBody *body, const cpVect point, cpVect *out);

void w_cpBodyGetVelocityAtWorldPoint(const cpBody *body, const cpVect point, cpVect *out);

void w_cpBodyGetVelocityAtLocalPoint(const cpBody *body, const cpVect point, cpVect *out);

void w_cpShapeGetBB(const cpShape *shape, cpBB *out);

void w_cpShapeUpdate(cpShape *shape, cpTransform transform, cpBB *out);

void w_cpShapeGetSurfaceVelocity(const cpShape *shape, cpVect *out);

void w_cpShapeGetFilter(const cpShape *shape, cpShapeFilter *out);

void w_cpShapeCacheBB(cpShape *shape, cpBB *out);

void w_cpCentroidForPoly(const int count, const cpVect *vects, cpVect *out);

cpFloat w_cpBBSegmentQuery(cpBB bb, cpVect a, cpVect b);

cpBool w_cpBBIntersectsSegment(cpBB bb, cpVect a, cpVect b);

void w_cpSpaceGetGravity(cpSpace *space, cpVect *out);

void w_cpPinJointGetAnchorA(const cpConstraint *constraint, cpVect *out);

void w_cpPinJointGetAnchorB(const cpConstraint *constraint, cpVect *out);

void w_cpSlideJointGetAnchorA(const cpConstraint *constraint, cpVect *out);

void w_cpSlideJointGetAnchorB(const cpConstraint *constraint, cpVect *out);

void w_cpPivotJointGetAnchorA(const cpConstraint *constraint, cpVect *out);

void w_cpPivotJointGetAnchorB(const cpConstraint *constraint, cpVect *out);

void w_cpGrooveJointGetGrooveA(const cpConstraint *constraint, cpVect *out);

void w_cpGrooveJointGetGrooveB(const cpConstraint *constraint, cpVect *out);

void w_cpGrooveJointGetAnchorB(const cpConstraint *constraint, cpVect *out);

void w_cpDampedSpringGetAnchorA(const cpConstraint *constraint, cpVect *out);

void w_cpDampedSpringGetAnchorB(const cpConstraint *constraint, cpVect *out);

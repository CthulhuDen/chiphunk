#include <chipmunk/chipmunk.h>

void w_cpSpaceGetGravity(const cpSpace *space, cpVect *out);

void w_cpvslerp(const cpVect v1, const cpVect v2, const cpFloat t, cpVect *out);

void w_cpvslerpconst(const cpVect v1, const cpVect v2, const cpFloat a, cpVect *out);

void w_cpBodyGetPosition(const cpBody *body, cpVect *out);

void w_cpBodyGetCenterOfGravity(const cpBody *body, cpVect *out);

void w_cpBodyGetVelocity(const cpBody *body, cpVect *out);

void w_cpBodyGetForce(const cpBody *body, cpVect *out);

void w_cpBodyGetRotation(const cpBody *body, cpVect *out);

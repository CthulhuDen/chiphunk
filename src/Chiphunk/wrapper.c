#include <wrapper.h>

void w_cpSpaceGetGravity(const cpSpace *space, cpVect *out)
{
  *out = cpSpaceGetGravity(space);
}

void w_cpvslerp(cpVect v1, cpVect v2, cpFloat t, cpVect *out)
{
  *out = cpvslerp(v1, v2, t);
}

void w_cpvslerpconst(cpVect v1, cpVect v2, cpFloat a, cpVect *out)
{
  *out = cpvslerpconst(v1, v2, a);
}

void w_cpBodyGetPosition(const cpBody *body, cpVect *out)
{
  *out = cpBodyGetPosition(body);
}

void w_cpBodyGetCenterOfGravity(const cpBody *body, cpVect *out)
{
  *out = cpBodyGetCenterOfGravity(body);
}

void w_cpBodyGetVelocity(const cpBody *body, cpVect *out)
{
  *out = cpBodyGetVelocity(body);
}

void w_cpBodyGetForce(const cpBody *body, cpVect *out)
{
  *out = cpBodyGetForce(body);
}

void w_cpBodyGetRotation(const cpBody *body, cpVect *out)
{
  *out = cpBodyGetRotation(body);
}

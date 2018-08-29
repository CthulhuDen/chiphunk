#include <wrapper.h>

void w_cpv(cpFloat x, cpFloat y, cpVect *out)
{
  *out = cpv(x, y);
}

void w_cpSpaceGetGravity(const cpSpace *space, cpVect *out)
{
  *out = cpSpaceGetGravity(space);
}

void w_cpBodyGetPosition(const cpBody *body, cpVect *out)
{
  *out = cpBodyGetPosition(body);
}

void w_cpBodyGetVelocity(const cpBody *body, cpVect *out)
{
  *out = cpBodyGetVelocity(body);
}

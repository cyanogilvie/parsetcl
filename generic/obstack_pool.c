#include <stdint.h>
#include <stdlib.h>
#include <time.h>
#include <threads.h>
#include <obstack.h>
#include "obstack_pool.h"

struct obstack_slot {
	struct obstack			ob;				// Must be first
	struct obstack_slot*	next;
	uint64_t				last_returned;
	void*					first_object;
};

struct obstack_pool {
	struct obstack_slot*	free;
	int						avail;
	uint64_t				oldest;
};

thread_local struct obstack_pool	t_obstack = {0};

uint64_t nanoseconds() // Time in nanoseconds, not necessarily monotonic or good for performance measurement (may drift)
{
	struct timespec	ts;
	timespec_get(&ts, TIME_UTC);
	return (uint64_t)ts.tv_sec * 1000000000 + ts.tv_nsec;
}


struct obstack* obstack_pool_get(enum obstack_pool_estimate est) //<<<
{
	struct obstack_slot*	slot;

	if (t_obstack.free) {
		slot = t_obstack.free;
		t_obstack.free = slot->next;
		slot->next = NULL;
		t_obstack.avail--;
		return (struct obstack*)slot;
	}

	slot = (struct obstack_slot*)obstack_chunk_alloc(sizeof(struct obstack_slot));
	switch (est) {
		case OBSTACK_POOL_SMALL:
			obstack_begin(&slot->ob, 12288-32);
			break;
		case OBSTACK_POOL_MEDIUM:
			obstack_begin(&slot->ob, 1048576-32);
			break;
		default:
			obstack_begin(&slot->ob, 12288-32);
	}
	slot->first_object = obstack_alloc(&slot->ob, 1);	// Record the first obstack allocation so we can release it when the obstack is returned

	return (struct obstack*)slot;
}

//>>>
void obstack_pool_release(struct obstack* ob) //<<<
{
	struct obstack_slot*	slot = (struct obstack_slot*)ob;
	const uint64_t			now = nanoseconds();

	obstack_free(&slot->ob, slot->first_object);
	slot->first_object = obstack_alloc(&slot->ob, 1);

	slot->next = t_obstack.free;
	slot->last_returned = now;

	if (t_obstack.free == NULL)
		t_obstack.oldest = now;

	t_obstack.free = slot;
	t_obstack.avail++;

	obstack_pool_groom(now);
}

//>>>
void obstack_pool_groom(uint64_t now) //<<<
{
	const int		min_pool = 10;		// Keep at least this many slots in the pool
	const uint64_t	horizon = now - 30*1000000000ULL;	// Free excess slots that are older than this

	if (
			t_obstack.avail > min_pool &&
			t_obstack.oldest < horizon
	) {
		int						i = 0;
		struct obstack_slot*	s = t_obstack.free;
		struct obstack_slot*	p = NULL;

		while (s) {
			if (++i > min_pool && s->last_returned > horizon) {
				// Free this slot and all trailing ones (which are guaranteed to be older)
				p->next = NULL;
				t_obstack.oldest = p->last_returned;
				t_obstack.avail = i-1;
				while (s) {
					struct obstack_slot*	next = s->next;
					obstack_free(&s->ob, NULL);
					obstack_chunk_free(s);
					s = next;
				}
				break;
			}
			p = s;
			s = s->next;
		}
	}
}

//>>>
void obstack_pool_shutdown() //<<<
{
	struct obstack_slot*	s = t_obstack.free;

	while (s) {
		struct obstack_slot*	next = s->next;
		obstack_free(&s->ob, NULL);
		obstack_chunk_free(s);
		s = next;
	}
	t_obstack.avail = 0;
}

//>>>

// vim: ft=c foldmethod=marker foldmarker=<<<,>>> ts=4 shiftwidth=4

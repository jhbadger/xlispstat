/*
 * Memory Allocator for Microsoft Windows.
 * Only allows blocks of up to 64K minus overhead to be allocated.
 *
 * This code is based on the GNU malloc.c included with GNU emacs and is
 * subject to the copyright restrictions listed therein. The original of
 * malloc.c is included with this distribution. (I hope this approach is
 * in accord with the license.)
 */

#ifdef _Windows
#include <mem.h>
extern char *NewSegment(void);
extern void FreeSegment(char *);
extern void abort(void);

extern int Exiting;

static void init_blocks(void);
static unsigned int find_block_index(unsigned long);
static void morecore(int);
char *Lmalloc(unsigned long);
char *Lrealloc(char *, unsigned long);
void Lfree(char *);
#else
#include <string.h>
#endif

/*
 * nextf[i] is the pointer to the next free block of size bsize[i].  The
 * smallest allocatable block is 8 bytes.  The overhead information will
 * go in the first long of the block, and the returned pointer will point
 * to the second.
 */

#define ISALLOC ((char) 0xf7)	/* magic byte that implies allocation */
#define ISFREE ((char) 0x54)	/* magic byte that implies free block */
				/* this is for error checking only */

/* What we have is a flag indicating whether memory is allocated, an index
   in nextf[], and a size field; to realloc() memory we copy size bytes.
   Since we are not allowing allocations of more than 64K, a short size
   field is adequate for holding the actual size of any block. The size
   field is only valid when the block is allocated.
   */

struct mhead {
  char mh_alloc;		/* ISALLOC or ISFREE */
  unsigned char mh_index;	/* index in nextf[] */
  unsigned short mh_size;	/* size */
};

/* Segments are connected in a linked list. The header is 16 bytes long and
   a trailer of 16 bytes is used. This should hopefully insure that far
   pointers to reasonable-sized structs will not wrap when incremented or
   decremented
   */

#define SEGSIZE 0x010000L              /* 64K */
#define SEGMAX ((int) (SEGSIZE - 32))  /* 64K - header - trailer */

struct shead {
  struct shead *sh_next;    /* pointer to next segment in list */
  unsigned short sh_start;  /* offset to first unallocated byte in segment */
  unsigned short sh_free;   /* amount of free space in segment */
  long pad1, pad2;          /* padding up to 16 byte boundary */
};

static struct shead *segs = 0;

/* BCC 4.0 seems to call malloc from its startup code (as do earlier     */
/* versions). But under 4.0 an error occurs on exit if I do GlobalFree   */
/* on the first segment that is created for this initial malloc. My best */
/* guess is that the cleanup code in BCC 4.0 calls GlobalFree directly   */
/* instead of free, resulting in trying to free a block already free'd.  */
/* So I record the first segment and don't free it in the cleanup code.  */
/* This whole grossness should vanish with Win32s.                       */

static struct shead *firstseg = 0;

/* Access free-list pointer of a block. It is stored at block + 4.
  This is not a field in the mhead structure because we want
  sizeof (struct mhead) to describe the overhead for when the block
  is in use, and we do not want the free-list pointer to count in that.
  */
#define CHAIN(a) \
  (*(struct mhead **) (sizeof (char *) + (char *) (a)))

/* nextf[i] is free list of blocks of size bsize[i].  */
#define MAXINDEX 156
static struct mhead *nextf[MAXINDEX];

/* bsize[i] is the size of blocks of category i.
   The shift up by one in bsize allows the range test in realloc to work.
   (This range test is no longer used -- it was wrong) */
static unsigned int blocksizes[MAXINDEX + 1] = {0};
unsigned int *bsize = blocksizes + 1;

/* Function to call to issue a warning. */
#ifdef _Windows
static void (*warnfunction) (char *);
#else
static void (*warnfunction) ();
#endif
#define WARNING(s) if (warnfunction) (*warnfunction)(s)

/* initialize the block size array */
static void init_blocks(void)
{
  register unsigned int i, j, k, m, n;

  for (m = 8, i = 0, k = 0; i < 9; i++)
    for (j = 0, n = m / 4; j < 4; j++, k++, m += n)
      bsize[k] = m;
  for (; k < MAXINDEX - 1; k++, m+= n)
    bsize[k] = m;
  m = SEGMAX;
  bsize[k] = m;
}

/* Figure out which nextf[] area to use for a given size */
static unsigned int find_block_index(n)
     register unsigned long n;
{
  register long m;
  register unsigned int i, j;

  n += sizeof(struct mhead); /* add 4-byte header */
  for (i = 0, m = 8; n > m && i < 9; i++, m <<= 1);
  j = (unsigned int) (((n - 1) >> i) - 4);
  return (i ? 1 + j + ((i - 1) << 2) : 0);
}

/* Initialize the allocator. */
void
malloc_init (warnfun)
     void (*warnfun) ();
{
  warnfunction = warnfun;
}

/* Clean up before shutdown */
void
malloc_cleanup()
{
  struct shead *s;

  while ((s = segs) != 0 && s != firstseg) {
    segs = s->sh_next;
    FreeSegment((char *) s);
  }
}

static void
morecore (nu)			/* ask system for more memory */
     register int nu;		/* size index to get more of  */
{
  register char *cp;
  register unsigned int siz;
  register struct shead *s, *nexts;
  static int inited = 0;

  if (! inited) {
    init_blocks();
    inited = 1;
  }

  /* check that nu is in range */
  if (nu < 0 || nu >= MAXINDEX) {
    WARNING("Can't allocate a block that large");
    return;
  }

  /* calculate the size we need */
  siz = bsize[nu];

  /* see if there is a segment with enough space */
  for (s = 0, nexts = segs; nexts; nexts = nexts->sh_next) {
    if (siz <= nexts->sh_free) {
      s = nexts;
      break;
    }
  }

  /* if we did not find one, get and initialize a new segment */
  if (! s) {
    s = (struct shead *) NewSegment();
    if (! s) return;
    s->sh_next = segs;
    segs = s;
    s->sh_free = SEGMAX;
    s->sh_start = 16;
    if (! firstseg) firstseg = segs;
  }

  /* double check the size */
  if (!s || s->sh_free < siz) return;

  /* get a new block from the segment and adjust the segment header */
  cp = ((char *) s) + s->sh_start;
  s->sh_start += siz;
  s->sh_free -= siz;

  /* save and initialize the new header */
  nextf[nu] = (struct mhead *) cp;
  ((struct mhead *) cp) -> mh_alloc = ISFREE;
  ((struct mhead *) cp) -> mh_index = nu;
  CHAIN ((struct mhead *) cp) = 0;
}

char *
Lmalloc (n)		/* get a block */
     unsigned long n;
{
  register struct mhead *p;
  register unsigned int nunits;

  /* reject if requested block is too large */
  if (n > SEGMAX - sizeof(struct mhead)) {
    WARNING("Can't allocate a block that large");
    return (0);
  }

  nunits = find_block_index(n);

  /* reject if too large -- should not happen with earlier test */
  if (nunits >= MAXINDEX) {
    WARNING("Can't allocate a block that large");
    return (0);
  }

  /* If there are no blocks of the appropriate size, go get some */
  /* COULD SPLIT UP A LARGER BLOCK HERE ... ACT */
  if (nextf[nunits] == 0)
    morecore (nunits);

  /* Get one block off the list, and set the new list head */
  if ((p = nextf[nunits]) == 0)
    return 0;
  nextf[nunits] = CHAIN (p);

  /* Check for free block clobbered */
  /* If not for this check, we would gobble a clobbered free chain ptr */
  /* and bomb out on the NEXT allocate of this size block */
  if (p -> mh_alloc != ISFREE || p -> mh_index != nunits)
    abort ();

  /* Fill in the header info */
  p -> mh_alloc = ISALLOC;
  p -> mh_size = (unsigned short) n;

  return (char *) (p + 1);
}

void Lfree (mem)
     char *mem;
{
  register struct mhead *p;
#ifdef _Windows
  if (Exiting) return;
#endif /* _Windows */
  {
    register char *ap = mem;

    if (ap == 0)
      return;

    p = (struct mhead *) ap - 1;
    if (p -> mh_alloc != ISALLOC)
      abort ();

  }
  {
    register unsigned int nunits = p -> mh_index;

    p -> mh_alloc = ISFREE;

    /* Put this block on the free list.  */
    CHAIN (p) = nextf[nunits];
    nextf[nunits] = p;
  }
}

char *
Lrealloc (mem, n)
     char *mem;
     register unsigned long n;
{
  register struct mhead *p;
  register unsigned long tocopy;
  register long nunits;

  /* Check for maximum block size */
  if (n > SEGMAX - sizeof(struct mhead)) {
    WARNING("Can't allocate a block that large");
    return(0);
  }

  /* if pointer is nil just malloc */
  if ((p = (struct mhead *) mem) == 0)
    return Lmalloc (n);

  /* find the header information */
  p--;
  nunits = p -> mh_index;
  tocopy = p -> mh_size;

  /* If desired size fits in current block, just mark its size as changed. */
  if (nunits == find_block_index(n))
    {
      p -> mh_size = (unsigned short) n;
      return mem;
    }

  if (n < tocopy)
    tocopy = n;

  {
    register char *new;
    if ((new = Lmalloc (n)) == 0)
      return 0;
    memcpy (new, mem, (size_t) tocopy);
    Lfree (mem);
    return new;
  }
}

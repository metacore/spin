#define PAGESIZE 8192

struct hdr {
    int from, len
};

void xmemcpy(long *dst, long *src, int n)
{
    while (n > 0) {
	*dst++ = *src++;
	n--;
    }
}

static void foo()
{
    ;
}
#define ASSERT_RANGE if (n >= 1024) {printf("diff range!!");foo();}

int diff (long *old, long *new, long *diff, int copy)
{
    int change_start = -1;
    int change_end = -1;
    int last_diff_idx = -1;
    int cont = 0;
    int n = 0;
    int i;
    for (i = 0; i < PAGESIZE/sizeof(long); i++) {
	if (old[i] != new[i]) {
	    if (change_start == -1) {
	      xxx:
		if (change_end != -1 && i-change_end < 4) {
		    cont = 1;
		}
		change_start = i;
	    }
	    if (copy) old[i] = new[i];
	} else {
	    if (change_start != -1) {
		if (cont) {
		    struct hdr *hdr = (struct hdr*)&diff[last_diff_idx];
		    int old_len = hdr->len;

		    hdr->len = i - hdr->from;
		    xmemcpy(diff+last_diff_idx+1+old_len,
			   new+hdr->from+old_len, 
			   hdr->len - old_len);
		    n += hdr->len - old_len;
		    ASSERT_RANGE;
		    cont = 0;
		} else {
		    struct hdr *hdr = (struct hdr*)&diff[n];
		    last_diff_idx = n;
		    hdr->from = change_start;
		    hdr->len = i - hdr->from;
		    xmemcpy(diff+n+1, new+change_start, hdr->len);
		    n += 1+hdr->len;
		    ASSERT_RANGE;
		}
		change_start = -1;
		change_end = i;
	    } else {
		while (old[i] == new[i]) {
		    i++;
		    if (i >= PAGESIZE/sizeof(long))
		      goto end;
		}
		goto xxx;
	    }
	}
    }
  end:
    ASSERT_RANGE;
    return n*8;
}


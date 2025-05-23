/*
 *	GLISS component simulator memory manager
 *
 *	This file is part of OTAWA
 *	Copyright (c) 2008, IRIT UPS.
 *
 *	GLISS is free software; you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation; either version 2 of the License, or
 *	(at your option) any later version.
 *
 *	GLISS is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with OTAWA; if not, write to the Free Software
 *	Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

/**
 * @defgroup memory Memory Module
 * A memory module is used to simulate the memory behaviour.
 * This module is mandatory and is only currently implemented by
 * the @ref io_mem .
 *
 * @author J. Barre, H. Casse, P. Sainrat
 *
 * @page csim_mem	Memory module for component simulation
 *
 * This is a memory module acting like the fast_mem module, but you can specify a callback function
 * to be called in case of an access at a given page or address
 */

/**
 * @def csim_address_t
 * This type represents a 32-bit address in memory space.
 * @ingroup memory
 */

/**
 * @typedef csim_memory_t
 * This type is used to represent a memory space.
 * @ingroup memory
 */

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <memory.h>
#include "mem.h"

#ifndef NDEBUG
#	define assertp(c, m)	\
		if(!(c)) { \
			fprintf(stderr, "assertion failure %s:%d: %s", __FILE__, __LINE__, m); \
			abort(); }
#else
#	define assertp(c, m)
#endif


/* callback infos, to be remembered as pages are created only when first accessed */

typedef struct csim_callback_info_t {
	csim_addr_t start;
	csim_addr_t end;
	csim_callback_fun_t callback_fun;
	void *callback_data ;
	struct csim_callback_info_t *next;
} csim_callback_info_t;

typedef struct {
	csim_callback_info_t *ptr;
	int is_changed;
} csim_callback_info_table_t;


/* MEMORY REPRESENTATION */

/* WARNING: constant below must be multiple of 8 bytes ! */
#define MEMORY_PAGE_SIZE CSIM_PAGE_SIZE		/** memory page size */
#define PRIMARYMEMORY_HASH_TABLE_SIZE 4096	/** primary hash table size */
#define SECONDARYMEMORY_HASH_TABLE_SIZE 16	/** secondary hash table size */

/*
 * Memory is allocated dynamically when needed.
 * The memory is organized as:
 *
 *  |-------------|
 *  |             |
 *  |-------------|
 *  |             |
 *  |-------------|
 *  |             |
 *  |-------------|
 *  |      -------|------->|-------|
 *  |-------------|        |   ----|---> list of pairs(address +
 *  |             |        |-------|                   MEMORY_PAGE_SIZE bytes)
 *  |-------------|        |       |
 *  |             |        |-------|
 *  |-------------|          ....
 *  |             |
 *  |-------------|
 *      ....
 *   ^ primary hastable       ^ secondary hashtable
 *
 * the hash method works as :
 *
 * memory address is hashed like that
 *
 *  ..----|----|------------|------------|
 *     4    3        2            1
 * 1 -> the address inside the page
 * 2 -> the address corresponding to the primary hash code
 * 3 -> the address corresponding to the secondary hash code
 * 4 -> inside the "list of pairs" described overhead
 */
typedef struct memory_page_table_entry_t  {
	csim_addr_t addr;
	struct memory_page_table_entry_t *next;
	uint8_t *storage;
	csim_callback_info_t *info;	/** callback function for IO, NULL if no callback */
} memory_page_table_entry_t;

typedef struct  {
	memory_page_table_entry_t *pte[SECONDARYMEMORY_HASH_TABLE_SIZE];
} secondary_memory_hash_table_t;

struct csim_memory_t {
	void* image_link; /**	link to a generic image data resource of the memory
							it permits to fetch informations about image structure
							via an optionnal external system */
	secondary_memory_hash_table_t *primary_hash_table[PRIMARYMEMORY_HASH_TABLE_SIZE];
	csim_callback_info_table_t callback_infos;	/** infos about callback functions */
#ifdef GLISS_MEM_SPY
	gliss_mem_spy_t spy_fun;	/** spy function */
	void *spy_data;				/** spy data */
#endif
};
typedef struct csim_memory_t memory_64_t;


#ifdef GLISS_MEM_SPY
/**
 * Default spy function: do nothing.
 */
static void csim_mem_default_spy(csim_memory_t *mem, csim_address_t addr, csim_size_t size, gliss_access_t access, void *data) {
}
#endif


/**
 * Compute hash level 1.
 * @param addr	Address to hash.
 * @return		Hash value.
 */
static uint32_t mem_hash1(csim_addr_t addr)  {
    return (addr / MEMORY_PAGE_SIZE) % PRIMARYMEMORY_HASH_TABLE_SIZE;
}


/**
 * Compute hash level 2.
 * @param addr	Address to hash.
 * @return		Hash value.
 */
static uint32_t mem_hash2(csim_addr_t addr)  {
    return (addr / MEMORY_PAGE_SIZE / PRIMARYMEMORY_HASH_TABLE_SIZE) % SECONDARYMEMORY_HASH_TABLE_SIZE;
}


/**
 * Initialize the passed memory.
 * @param mem	Memory to initialize.
 * @ingroup memory
 */
static void csim_mem_init(memory_64_t *mem) {

	/* initialize memory */
	memset(mem->primary_hash_table,0,sizeof(mem->primary_hash_table));
	mem->image_link = NULL;
	mem->callback_infos.ptr = 0;
	mem->callback_infos.is_changed = 0;

	/* initialize spy */
	#	ifdef ARM_MEM_SPY
	mem->spy_fun = arm_mem_default_spy;
	mem->spy_data = 0;
	#	endif
}


/**
 * Build a new memory handler.
 * @return	Memory handler or NULL if there is not enough memory.
 * @ingroup memory
 */
csim_memory_t *csim_mem_new(void) {
	memory_64_t *mem;
	mem = (memory_64_t *)malloc(sizeof(memory_64_t));
	if(mem != NULL)
		csim_mem_init(mem);
	return (csim_memory_t *)mem;
}


#ifdef CSIM_MEM_SPY
/**
 * Set the spy function on the given memory. This function will be called
 * at each memory access with the details of the memory transaction.
 * @param mem	Current memory.
 * @param fun	Function called at each memory access.
 * @param data	Data passed as the last argument when function fun is called.
 */
void csim_mem_set_spy(csim_memory_t *mem, csim_mem_spy_t fun, void *data) {
	assert(mem);
	if(!fun) {
		mem->spy_fun = gliss_mem_default_spy;
		mem->spy_data = 0;
	}
	else {
		mem->spy_fun = fun;
		mem->spy_data = data;
	}
}
#endif


void csim_mem_clean(csim_memory_t *memory) {
	int i,j;
	secondary_memory_hash_table_t *secondary_hash_table;
	memory_page_table_entry_t *pte;
	memory_page_table_entry_t *nextpte;

	/* get right type */
	memory_64_t *mem64 = (memory_64_t *)memory;

	for (i=0; i<PRIMARYMEMORY_HASH_TABLE_SIZE;i++)  {
		secondary_hash_table = mem64->primary_hash_table[i];
		if(secondary_hash_table) {
			for(j=0;j<SECONDARYMEMORY_HASH_TABLE_SIZE;j++) {
				pte=secondary_hash_table->pte[j];
				while (pte) {
					nextpte=pte->next;
					free(pte->storage);
					free(pte);	/* freeing each page */
					pte=nextpte;
				}
			}
			free(secondary_hash_table); /* freeing each secondary hash table */
		}
	}

	/* free the callback list */
	memory->callback_infos.ptr = 0;
	csim_callback_info_t *info_ptr = memory->callback_infos.ptr;
	csim_callback_info_t *next_ptr = 0;
	while (info_ptr) {
		next_ptr = info_ptr->next;
		free(info_ptr);
		info_ptr = next_ptr;
	}

	/* free the memory */
	free(mem64); 	/* freeing the primary hash table */
}


/**
 * Free and delete the given memory.
 * @param memory	Memory to delete.
 * @ingroup memory
 */
void csim_mem_delete(csim_memory_t *memory) {
	csim_mem_clean(memory);
	memory_64_t *mem64 = (memory_64_t *)memory;
	free(mem64); 	/* freeing the primary hash table */
}


/**
 * Reset the passed memory.
 * @param memory	Memory to reset.
 * @ingroup memory
 */
void arm_mem_reset(csim_memory_t *memory) {
	csim_mem_clean(memory);
	memory_64_t *mem64 = (memory_64_t *)memory;
	csim_mem_init(mem64);
}


/**
 * Copy the current memory.
 * @param memory	Memory to copy.
 * @return			Copied memory or null if there is not enough memory.
 * @ingroup memory
 */
/* !!TODO!! implement copy of callback info */
csim_memory_t *csim_mem_copy(csim_memory_t *memory) {
	int i,j;
	memory_64_t *mem = memory, *target;

	/* allocate memory */
	target = csim_mem_new();
	if(target == NULL)
		return NULL;

	/* copy memory */
	for(i=0;i<PRIMARYMEMORY_HASH_TABLE_SIZE;i++) {
		secondary_memory_hash_table_t *secondary_hash_table = mem->primary_hash_table[i];
		if(secondary_hash_table) {
			for(j=0;j<SECONDARYMEMORY_HASH_TABLE_SIZE;j++) {
				memory_page_table_entry_t *pte=secondary_hash_table->pte[i];
				if(pte) {
					do {
						csim_mem_write(target, pte->addr, pte->storage, MEMORY_PAGE_SIZE);
					} while((pte=pte->next) != 0);
				}
			}
		}
	}
	return target;
}


/**
 * Look for a page in memory.
 * @param mem	Memory to work on.
 * @param addr	Address of the looked page.
 * @ingroup memory
 */
static memory_page_table_entry_t *mem_search_page(memory_64_t *mem, csim_addr_t addr) {
	uint32_t h1;
	uint32_t h2;
	secondary_memory_hash_table_t *secondary_hash_table;
	memory_page_table_entry_t *pte;

	/* computes the first adress of the page */
	addr = addr - (addr%MEMORY_PAGE_SIZE);
	h1 = mem_hash1(addr);
	secondary_hash_table = mem->primary_hash_table[h1];

	/* if the secondary hash table exists */
	if(secondary_hash_table) {

		h2 = mem_hash2(addr);
		pte = secondary_hash_table->pte[h2];

		/* search the page entry */
		if(pte) {
			do  {
				if(pte->addr==addr)
                	return pte;
			} while((pte=pte->next)!=0);
		}
	}
	return NULL;
}


/**
 * Get a secondary page table.
 * @parm mem	Memory to work on.
 * @param addr	Address of the page.
 */
static secondary_memory_hash_table_t* mem_get_secondary_hash_table(memory_64_t *mem, csim_addr_t addr) {
	uint32_t h1;
	secondary_memory_hash_table_t* secondary_hash_table;

	/* try to fetch the secondary hashtable */
	h1 = mem_hash1(addr);
	secondary_hash_table = mem->primary_hash_table[h1];

	/* if the secondary hashtable does'nt exists */
	if(!secondary_hash_table) {
		/* allocation of the secondary hashtable */
		secondary_hash_table = (secondary_memory_hash_table_t *)
			calloc(sizeof(secondary_memory_hash_table_t),1);

		assertp(secondary_hash_table != NULL,
			"Failed to allocate memory in mem_get_secondary_hash_table\n");
		mem->primary_hash_table[h1]=secondary_hash_table;
	}

	return secondary_hash_table;
}


static void update_callback_infos(csim_memory_t *mem);
static csim_callback_info_t *get_callback_info(csim_callback_info_table_t *infos, csim_addr_t addr);

/**
 * Get the page matching the given address and create it if it does not exist.
 * @parm mem	Memory to work on.
 * @param addr	Address of the page.
 */
static memory_page_table_entry_t *mem_get_page(memory_64_t *mem, csim_addr_t addr) {
	memory_page_table_entry_t *pte;
	uint32_t h2; /* secondary hash table entry # value */
	secondary_memory_hash_table_t *secondary_hash_table;

	/* is there any new callback info? */
	if (mem->callback_infos.is_changed)
		update_callback_infos(mem);

	/* search the page */
	addr = addr - (addr%MEMORY_PAGE_SIZE);
	pte = mem_search_page(mem,addr);

	/* if the page doesn't yet exists */
	if(!pte)  {
		secondary_hash_table = mem_get_secondary_hash_table(mem, addr);
		h2 = mem_hash2(addr);

		/* allocation of the page entry descriptor */
		pte = (memory_page_table_entry_t *)malloc(sizeof(memory_page_table_entry_t));
		assertp(pte != NULL, "Failed to allocate memory in mem_get_page\n");
		pte->addr = addr;

		/* allocation of the page */
		pte->storage = (uint8_t *)calloc(MEMORY_PAGE_SIZE,1);
		assertp(pte->storage != NULL, "Failed to allocate memory in mem_get_page\n");

		/* set callback function */
		pte->info = get_callback_info(&mem->callback_infos, addr);

		/* adding the memory page to the list of memory page size entry*/
		pte->next = secondary_hash_table->pte[h2];
		secondary_hash_table->pte[h2]=pte;
	}
	return pte;
}


/**
 * Write a buffer into memory.
 * @param memory	Memory to write into.
 * @param address	Address in memory to write to.
 * @param buffer	Buffer address in host memory.
 * @param size		Size of the buffer to write.
 * @ingroup memory
 */
void csim_mem_write(csim_memory_t *memory, csim_addr_t address, void *buffer, size_t size) {
	if(size>0) {
		memory_64_t *mem = (memory_64_t *)memory;
		uint32_t offset = address % MEMORY_PAGE_SIZE;
		memory_page_table_entry_t *pte = mem_get_page(mem, address);
        uint32_t sz = MEMORY_PAGE_SIZE - offset;
        if(size > sz) {
			memcpy(pte->storage+offset, buffer, sz);
			size -= sz;
			address += sz;
			buffer = (uint8_t *)buffer + sz;
			if(size>=MEMORY_PAGE_SIZE) {
				do {
					pte = mem_get_page(mem, address);
					memcpy(pte->storage, buffer, MEMORY_PAGE_SIZE);
					size -= MEMORY_PAGE_SIZE;
					address += MEMORY_PAGE_SIZE;
					buffer = (uint8_t *)buffer + MEMORY_PAGE_SIZE;
				} while(size >= MEMORY_PAGE_SIZE);
			}
			if(size > 0) {
				pte=mem_get_page(mem, address);
				memcpy(pte->storage, buffer, size);
			}
        }
		else
			memcpy(pte->storage + offset, buffer, size);
    }

#	ifdef GLISS_MEM_SPY
    	memory->spy_fun(memory, address, size, gliss_access_write, memory->spy_data);
#	endif
}


/**
 * Read the memory into the given buffer.
 * @param memory	Memory to read in.
 * @param address	Address of the data to read.
 * @param buffer	Buffer to write data in.
 * @param size		Size of the data to read.
 * @ingroup memory
 */
void csim_mem_read(csim_memory_t *memory, csim_addr_t address, void *buffer, size_t size) {
	if(size > 0) {
		memory_64_t *mem = (memory_64_t *) memory;
		uint32_t offset = address % MEMORY_PAGE_SIZE;
		memory_page_table_entry_t *pte = mem_get_page(mem, address);
		uint32_t sz = MEMORY_PAGE_SIZE - offset;
		if(size > sz) {
			memcpy(buffer, pte->storage + offset, sz);
			size -= sz;
            address += sz;
			buffer = (uint8_t *)buffer + sz;
			if(size >= MEMORY_PAGE_SIZE) {
				do {
					pte = mem_get_page(mem, address);
					memcpy(buffer, pte->storage, MEMORY_PAGE_SIZE);
					size -= MEMORY_PAGE_SIZE;
					address += MEMORY_PAGE_SIZE;
					buffer = (uint8_t *)buffer + MEMORY_PAGE_SIZE;
				} while(size >= MEMORY_PAGE_SIZE);
			}
			if(size>0) {
				pte = mem_get_page(mem, address);
				memcpy(buffer, pte->storage, size);
			}
		}
		else
			memcpy(buffer, pte->storage + offset, size);
    }

#	ifdef GLISS_MEM_SPY
    	memory->spy_fun(memory, address, size, gliss_access_read, memory->spy_data);
#	endif
}


/**
 * Read an 8-bit integer.
 * @param memory	Memory to work with.
 * @param address	Address of integer to read.
 * @return			Read integer.
 * @ingroup memory
 */
uint8_t csim_mem_read8(csim_memory_t *memory, csim_addr_t address) {
	memory_64_t *mem = (memory_64_t *)memory;
	csim_addr_t offset = address % MEMORY_PAGE_SIZE;
	memory_page_table_entry_t *pte = mem_get_page(mem, address);

	/* support of callback */
	uint8_t res;
	if(pte->info != NULL)
		pte->info->callback_fun(address, 1, &res, CSIM_MEM_READ, pte->info->callback_data);

	/* normal case */
	else
		res = pte->storage[offset];
#	ifdef CSIM_MEM_SPY
    	memory->spy_fun(memory, address, sizeof(res), gliss_access_read, memory->spy_data);
#	endif
    return res;
}


/**
 * Read a 16-bit integer.
 * @param memory	Memory to work with.
 * @param address	Address of integer to read.
 * @return			Read integer.
 * @ingroup memory
 */
uint16_t csim_mem_read16(csim_memory_t *memory, csim_addr_t address) {
	memory_64_t *mem = (memory_64_t *)memory;
	uint16_t res;

	/* get page */
	csim_addr_t offset = address % MEMORY_PAGE_SIZE;
	memory_page_table_entry_t *pte=mem_get_page(mem, address);

	/* callback case */
	if (pte->info)
		pte->info->callback_fun(address, 2, &res, CSIM_MEM_READ, pte->info->callback_data);

	/* normal case */
	else {
		uint8_t *p = pte->storage + offset;

		/* aligned */
		if((address & 0x00000001) == 0)
			res = *(uint16_t *)p;	// aligned

		/* not aligned */
		else {
			union {
				uint8_t bytes[2];
				uint16_t half;
			} val;
			val.bytes[0] = p[0];
			val.bytes[1] = p[1];
			res = val.half;
		}
	}

	/* return result */
#	ifdef CSIM_MEM_SPY
    	memory->spy_fun(memory, address, sizeof(res), csim_access_read, memory->spy_data);
#	endif
    return res;
}


/**
 * Read a 32-bit integer.
 * @param memory	Memory to work with.
 * @param address	Address of integer to read.
 * @return			Read integer.
 * @ingroup memory
 */
uint32_t csim_mem_read32(csim_memory_t *memory, csim_addr_t address) {
	memory_64_t *mem = (memory_64_t *)memory;
	uint32_t res;

	/* get page */
	csim_addr_t offset = address % MEMORY_PAGE_SIZE;
	memory_page_table_entry_t *pte=mem_get_page(mem, address);

	/* callback case */
	if (pte->info)
		pte->info->callback_fun(address, 4, &res, CSIM_MEM_READ, pte->info->callback_data);

	/* normal case */
	else {
		uint8_t *p = pte->storage + offset;

		/* aligned */
		if((address & 0x00000003) == 0)
			res = *(uint32_t *)p;

		/* not aligned */
		else {
			union {
				uint8_t bytes[4];
				uint32_t word;
			} val;
			val.bytes[0] = p [0];
			val.bytes[1] = p [1];
			val.bytes[2] = p [2];
			val.bytes[3] = p [3];
			res = val.word;
		}
	}

	/* return result */
#	ifdef CSIM_MEM_SPY
    	mem->spy_fun(mem, address, sizeof(res), gliss_access_read, mem->spy_data);
#	endif
    return res;
}


/**
 * Read a 64-bit integer.
 * @param memory	Memory to work with.
 * @param address	Address of integer to read.
 * @return			Read integer.
 * @ingroup memory
 */
uint64_t gliss_mem_read64(csim_memory_t *memory, csim_addr_t address) {
	memory_64_t *mem = (memory_64_t *)memory;
	uint64_t res;
	union {
		uint8_t bytes[8];
		uint64_t dword;
	} val;

	/* get page */
	csim_addr_t offset = address % MEMORY_PAGE_SIZE;
	memory_page_table_entry_t *pte=mem_get_page(mem, address);

	/* callback case */
	if (pte->info)
		pte->info->callback_fun(address, 8, &res, CSIM_MEM_READ, pte->info->callback_data);

	/* normal case */
	else {
		uint8_t *p = pte->storage + offset;

		/* aligned */
		if((address & 0x00000007) == 0)
			res = *(uint64_t *)p;

		/* not aligned */
		else {
			memcpy(val.bytes, p, 8);
			res = val.dword;
		}
	}

	// return result
#	ifdef CSIM_MEM_SPY
    	mem->spy_fun(mem, address, sizeof(res), gliss_access_read, mem->spy_data);
#	endif
    return res;
}


/**
 * Read a float value.
 * @param memory	Memory to work with.
 * @param address	Address of float to read.
 * @return			Read float.
 * @ingroup memory
 */
float csim_mem_readf(csim_memory_t *memory, csim_addr_t address) {
	union {
		uint32_t i;
		float f;
	} val;
	val.i = csim_mem_read32(memory, address);
	return val.f;
}



/**
 * Read a double float value.
 * @param memory	Memory to work with.
 * @param address	Address of float to read.
 * @return			Read float.
 * @ingroup memory
 */
double csim_mem_readd(csim_memory_t *memory, csim_addr_t address) {
	union {
		uint64_t i;
		double f;
	} val;
	val.i = gliss_mem_read64(memory, address);
	return val.f;
}


/**
 * Read a long double float value.
 * @param memory	Memory to work with.
 * @param address	Address of float to read.
 * @return			Read float.
 * @ingroup memory
 */
long double csim_mem_readld(csim_memory_t *memory, csim_addr_t address) {
	assertp(0, "not implemented !");
}


/**
 * Write an 8-bit integer in memory.
 * @param memory	Memory to write in.
 * @param address	Address to write integer to.
 * @param val		Integer to write.
 * @ingroup memory
 */
void csim_mem_write8(csim_memory_t *memory, csim_addr_t address, uint8_t val) {
	memory_64_t *mem = (memory_64_t *)memory;

	/* get page */
	csim_addr_t offset = address % MEMORY_PAGE_SIZE;
	memory_page_table_entry_t *pte = mem_get_page(mem, address);

	/* callback case */
	if(pte->info != NULL)
		pte->info->callback_fun(address, 1, &val, CSIM_MEM_WRITE, pte->info->callback_data);

	/* normal case */
	else
		pte->storage[offset] = val;

	/* support spy */
#	ifdef GLISS_MEM_SPY
    	mem->spy_fun(mem, address, sizeof(val), csim_access_write, mem->spy_data);
#	endif
}


/**
 * Write a 16-bit integer in memory.
 * @param memory	Memory to write in.
 * @param address	Address to write integer to.
 * @param val		Integer to write.
 * @ingroup memory
 */
void gliss_mem_write16(csim_memory_t *memory, csim_addr_t address, uint16_t val) {
	memory_64_t *mem = (memory_64_t *)memory;

	/* get page */
	csim_addr_t offset = address % MEMORY_PAGE_SIZE;
	memory_page_table_entry_t *pte = mem_get_page(mem, address);

	/* callback case */
	if (pte->info != NULL)
		pte->info->callback_fun(address, 2, &val, CSIM_MEM_WRITE, pte->info->callback_data);

	/* normal case */
	else {
		uint16_t *q = (uint16_t *)(pte->storage + offset);

		/* aligned */
		if((address & 0x00000001) == 0)
			*q = val;

		/* unaligned */
		else {
			union val_t {
				uint8_t bytes[2];
				uint16_t half;
			} *p = (union val_t *)&val;
			memcpy(q, p->bytes, 2);
		}
	}

	/* spy access */
#	ifdef GLISS_MEM_SPY
    	mem->spy_fun(mem, address, sizeof(val), gliss_access_write, mem->spy_data);
#	endif
}


/**
 * Write a 32-bit integer in memory.
 * @param memory	Memory to write in.
 * @param address	Address to write integer to.
 * @param val		Integer to write.
 * @ingroup memory
 */
void csim_mem_write32(csim_memory_t *memory, csim_addr_t address, uint32_t val) {
	memory_64_t *mem = (memory_64_t *)memory;

	/* compute address */
	csim_addr_t offset = offset = address % MEMORY_PAGE_SIZE;
	memory_page_table_entry_t *pte = mem_get_page(mem, address);

	/* callback case */
	if (pte->info)
		pte->info->callback_fun(address, 4, &val, CSIM_MEM_WRITE, pte->info->callback_data);

	/* normal case */
	else {
		uint32_t *q = (uint32_t *)(pte->storage + offset);

		/* aligned */
		if((address & 0x00000003) == 0)
			*q = val;

		/* not aligned */
		else {
			union val_t {
				uint8_t bytes[4];
				uint32_t word;
			} *p = (union val_t *)&val;
			memcpy(q, p->bytes, 4);
		}
	}

	/* spy access */
#	ifdef GLISS_MEM_SPY
    	mem->spy_fun(mem, address, sizeof(val), gliss_access_write, mem->spy_data);
#	endif
}


/**
 * Write a 64-bit integer in memory.
 * @param memory	Memory to write in.
 * @param address	Address to write integer to.
 * @param val		Integer to write.
 * @ingroup memory
 */
void csim_mem_write64(csim_memory_t *memory, csim_addr_t address, uint64_t val) {
	memory_64_t *mem = (memory_64_t *)memory;

	/* compute address */
	csim_addr_t offset = address % MEMORY_PAGE_SIZE;
	memory_page_table_entry_t *pte = mem_get_page(mem, address);

	/* callback case */
	if(pte->info != NULL)
		pte->info->callback_fun(address, 8, &val, CSIM_MEM_WRITE, pte->info->callback_data);

	/* normal case */
	else {
		uint64_t *q = (uint64_t *)(pte->storage + offset);

		/* aligned */
		if((address & 0x00000007) == 0)
			*q = val;

		/* not aligned */
		else {
			union val_t {
				uint8_t bytes[8];
				uint64_t dword;
			} *p = (union val_t *)&val;
			memcpy(q, p->bytes, 8);
		}
	}

	/* spy access */
#	ifdef GLISS_MEM_SPY
    	mem->spy_fun(mem, address, sizeof(val), gliss_access_write, mem->spy_data);
#	endif
}


/**
 * Write a float in memory.
 * @param memory	Memory to write in.
 * @param address	Address to write float to.
 * @param val		Float to write.
 * @ingroup memory
 */
void csim_mem_writef(csim_memory_t *memory, csim_addr_t address, float val) {
	union {
		uint32_t i;
		float f;
	} v;
	v.f = val;
	csim_mem_write32(memory, address, v.i);
}


/**
 * Write a double float in memory.
 * @param memory	Memory to write in.
 * @param address	Address to write float to.
 * @param val		Float to write.
 * @ingroup memory
 */
void csim_mem_writed(csim_memory_t *memory, csim_addr_t address, double val) {
	union {
		uint64_t i;
		double f;
	} v;
	v.f = val;
	csim_mem_write64(memory, address, v.i);
}


/**
 * Write a double float in memory.
 * @param memory	Memory to write in.
 * @param address	Address to write float to.
 * @param val		Float to write.
 * @ingroup memory
 */
void csim_mem_writeld(csim_memory_t *memory, csim_addr_t address, long double val) {
	assertp(0, "not implemented");
}


/**
 * Search the given callback info list if there's anything concerning the given address,
 * return the address of callback information, 0 if no callback is defined for that address.
 * @param infos		Callback information table.
 * @param addr		Page addresse.
 */
static csim_callback_info_t *get_callback_info(csim_callback_info_table_t *infos, csim_addr_t addr)
{
	csim_callback_info_t *ptr = infos->ptr;
	while(ptr) {
		if( ((ptr->start <= addr) && (addr <= ptr->end))
		||  ((ptr->start <= (addr+MEMORY_PAGE_SIZE-1)) && ((addr+MEMORY_PAGE_SIZE-1) <= ptr->end)))
			return ptr;
		ptr = ptr->next;
	}
	return 0;
}


/**
 * Update callback infos for the given memory, for all already created pages
 * we set correctly the callback function address accordingly to the callback infos of the given memory.
 */
static void update_callback_infos(csim_memory_t *mem) {
	int i, j;

	/* go through pages */
	for (i = 0 ; i < PRIMARYMEMORY_HASH_TABLE_SIZE ; i++) {
		secondary_memory_hash_table_t *secondary_hash_table = mem->primary_hash_table[i];
		if (secondary_hash_table) {
			for (j = 0 ; j < SECONDARYMEMORY_HASH_TABLE_SIZE ; j++) {
				memory_page_table_entry_t *pte = secondary_hash_table->pte[j];
				if (pte) {
					do {
						/* get callback info for beginning of page, let's hope the whole page has the same callback function */
						pte->info = get_callback_info(&mem->callback_infos, pte->addr);
					} while ((pte=pte->next) != 0);
				}
			}
		}
	}
	/* nothing more to update */
	mem->callback_infos.is_changed = 0;
}


/**
 * set a callback function for a specified range of memory supposed to be used to map an IO peripheric.
 * As memory is divided in pages, the range will be extended to all the pages covering the given range.
 * Warning: overlap could happen between the normal memory and the bypassed one
 * @param mem		Memory to bypass
 * @param start		physical address of the start of the range to bypass
 * @param end		physical address of the end of the range to bypass
 * @param f		callback function bypassing the usual behavior
 * @ingroup memory
 */
void csim_set_range_callback(csim_memory_t *mem, csim_addr_t start, csim_addr_t end, csim_callback_fun_t f, void* data){
	/* store the infos in callback infos table */

	/* create new entry */
	csim_callback_info_t *new_info = malloc(sizeof(csim_callback_info_t));
	assertp(new_info, "malloc error for gliss_callback_info_t");
	new_info->start = start;
	new_info->end = end;
	new_info->callback_fun = f;
	new_info->callback_data = data ;

	/* link info with page */
	memory_page_table_entry_t *pte = mem_get_page(mem, start);
	assert(pte != NULL);
	pte->info = new_info;

	/* insert at beginning of the current list */
	new_info->next = mem->callback_infos.ptr;
	mem->callback_infos.ptr = new_info;

	/* signal we have to update already created pages */
	//mem->callback_infos.is_changed = 1;
}

/**
 * Remove the callback for the given range of memory.
 * @param mem	Memory to set.
 * @param start	Start address (rounded to the page).
 * @param end	End address (rounded to the page).
 */
void csim_unset_range_callback(csim_memory_t *mem, csim_addr_t start, csim_addr_t end) {
	csim_addr_t startp = (start + MEMORY_PAGE_SIZE - 1) & ~(MEMORY_PAGE_SIZE - 1);
	csim_addr_t endp = (end + MEMORY_PAGE_SIZE - 1) & ~(MEMORY_PAGE_SIZE - 1);
	csim_addr_t a;

	// synchronize callback information
	if (mem->callback_infos.is_changed)
		update_callback_infos(mem);

	// unset concerned page (beware the end address at 0xfffff000)
	a = startp;
	while(1) {
		memory_page_table_entry_t *page = mem_search_page(mem, a);
		if(page != NULL)
			page->info = NULL;
		if(a == endp)
			break;
		a = a + MEMORY_PAGE_SIZE;
	}
}


/**
 * Get the callback data corresponding to the given address.
 * @param mem	Memory to work with.
 * @param a		Examined address.
 * @return		Callback if any, NULL else.
 */
void *csim_get_callback_data(csim_memory_t *mem, csim_addr_t a) {

	/* synchronize callback information */
	if (mem->callback_infos.is_changed)
		update_callback_infos(mem);

	/* get the page */
	memory_64_t *m = (memory_64_t *)mem;
	memory_page_table_entry_t *pte = mem_get_page(m, a);

	/* get callback data */
	if(pte->info == NULL)
		return NULL;
	else
		return pte->info->callback_data;
}


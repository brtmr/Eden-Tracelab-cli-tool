#include<stdint.h>
#define _BSD_SOURCE
#define __USE_BSD
#include<endian.h>

#include"fastconvert.h"

uint16_t w16be(char* input){
    return be16toh(*((uint16_t*)input));
}

uint32_t w32be(char* input){
    return be32toh(*((uint32_t*)input));
}

uint64_t w64be(char* input){
    return be64toh(*((uint64_t*)input));
}

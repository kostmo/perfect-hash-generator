#include <stdlib.h>
#include <string.h>

#include "utils.h"

#include "generated_values.h"
#include "lookup.h"


Fnv32_t fnv_32a_numeric_buf(LongNumberBuffer buf, Fnv32_t initial_basis) {
    return fnv_32a_buf(buf.bytes, buf.size, initial_basis);
}

int countRequiredBytes(long num) {

    int count = 0;
    while (num) {
        count++;
        num >>= 8;
    }

    return count;
}

LongNumberBuffer convertToBytes(long num) {

    int count = countRequiredBytes(num);

    LongNumberBuffer myOutput = { .bytes = {0}, .size = count };

    for (int i = count - 1; i >= 0; i--) {

        char chunk = num & 0xff;
        myOutput.bytes[i] = chunk;

        num >>= 8;
    }

    return myOutput;
}

void printChunks(LongNumberBuffer chunks) {

    for (int i=0; i<chunks.size; i++) {

        printf("%02x", chunks.bytes[i]);
    }
    printf("\n");
}




void read_int_pairs(int keyArray[], GENERATED_VALUES_TYPE valueArray[], int count, const char* file_name) {
    FILE* file = fopen (file_name, "r");

    int i=0;
    while (!feof (file)) {  
        fscanf (file, "%d,%d", &keyArray[i], &valueArray[i]);
        i++;
    }

    fclose (file);
}


void read_string_pairs(char* keyArray[], GENERATED_VALUES_TYPE valueArray[], int count, const char* file_name) {
    FILE* file = fopen (file_name, "r");

    int i=0;
    while (!feof (file)) {

        char* line = malloc(100);

        if (fgets(line, 100, file) != NULL) {

            const char delimiter[2] = ",";

            char *token;


            /* get the first token */
            token = strtok(line, delimiter);

            keyArray[i] = token;


            /* get the second token */
            token = strtok(NULL, delimiter);

            valueArray[i] = atoi(token);

        } else {
            break;
        }

        i++;
    }

    fclose (file);
}


bool verify_int_key_lookup_correctness(const char* csv_file_name) {

    int input_count = sizeof(HASHED_VALUES)/sizeof(HASHED_VALUES[0]);

    int keyArray[input_count];
    GENERATED_VALUES_TYPE expectedValues[input_count];
    read_int_pairs(keyArray, expectedValues, input_count, csv_file_name);


    printf("input_count: %d\n", input_count);
    for (int i=0; i<input_count; i++) {
        printf("=========================\n");
        printf("Iteration: %d\n", i);

        printf("Key: %d\n", keyArray[i]);
        int value_position = lookup(convertToBytes(keyArray[i]));
        printf("Value position: %d\n", value_position);

        GENERATED_VALUES_TYPE expected_value = expectedValues[i];

        if (expected_value != HASHED_VALUES[value_position]) {
            printf("Expected value: %d; Actual value: %d\n", expected_value, HASHED_VALUES[value_position]);
            return false;
        }
    }

    return true;
}


bool verify_string_key_lookup_correctness(const char* csv_file_name) {

    int input_count = sizeof(HASHED_VALUES)/sizeof(HASHED_VALUES[0]);

    char* keyArray[input_count];
    GENERATED_VALUES_TYPE expectedValues[input_count];
    read_string_pairs(keyArray, expectedValues, input_count, csv_file_name);


    printf("input_count: %d\n", input_count);
    for (int i=0; i<input_count; i++) {
        printf("=========================\n");
        printf("Iteration: %d\n", i);

        printf("Key: %s\n", keyArray[i]);
        int value_position = lookup_str(keyArray[i]);
        printf("Value position: %d\n", value_position);

        GENERATED_VALUES_TYPE expected_value = expectedValues[i];

        if (expected_value != HASHED_VALUES[value_position]) {
            printf("Expected value: %d; Actual value: %d\n", expected_value, HASHED_VALUES[value_position]);
            return false;
        }
    }

    return true;
}




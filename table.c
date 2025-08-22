#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"

#define TABLE_MAX_LOAD 0.9

#define SWAP(type, a, b) do {type tmp = a; a = b; b = tmp;} while (false)

void initTable(Table* table) {
    table->count = 0;
    table->capacity = 0;
    table->entries = NULL;
}

void freeTable(Table* table) {
    FREE_ARRAY(Entry, table->entries, table->count);
    initTable(table);
}

static Entry* findEntry(Entry* entries, int capacity, ObjString* key, bool nearestEmpty) {
    // nearestEmpty true: finds entry or nearest empty, false: finds entry if possible
    uint32_t index = key->hash % capacity;
    int probeSequenceLength = 0;

    for (;;) {
        Entry* entry = &entries[index];
        if (entry->key == NULL && IS_NIL(entry->value)) {
            return entry;
        } else if (!nearestEmpty && probeSequenceLength > entry->probeSequenceLength) {
            return NULL;
        } else if (entry->key == key) {
            return entry;
        }

        index = (index + 1) % capacity;
        probeSequenceLength++;
    }
}

static void adjustCapacity(Table* table, int capacity) {
    Entry* entries = ALLOCATE(Entry, capacity);
    for (int i = 0; i < capacity; i++) {
        entries[i].key = NULL;
        entries[i].value = NIL_VAL;
        entries[i].probeSequenceLength = 0;
    }

    table->count = 0;
    for (int i = 0; i < table->capacity; i++) {
        Entry* entry = &table->entries[i];
        if (entry->key == NULL) continue; // Ignore tombstones and empty entries
        Entry* dest = findEntry(entries, capacity, entry->key, true);
        dest->key = entry->key;
        dest->value = entry->value;
        dest->probeSequenceLength = entry->probeSequenceLength;
        table->count++;
    }

    FREE_ARRAY(Entry, table->entries, table->capacity);
    table->entries = entries;
    table->capacity = capacity;
}

bool tableGet(Table* table, ObjString* key, Value* value) {
    if (table->count == 0) return false;

    Entry* entry = findEntry(table->entries, table->capacity, key, false);
    if (entry == NULL || entry->key == NULL) return false;

    *value = entry->value;
    return true;
}

bool tableSet(Table* table, ObjString* key, Value value) {
    if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
        int capacity = GROW_CAPACITY(table->capacity);
        adjustCapacity(table, capacity);
    }

    Entry* entry = findEntry(table->entries, table->capacity, key, false);
    if (entry == NULL || entry->key == NULL) { // Is it a new key?
        uint32_t index = key->hash % table->capacity;
        int probeSequenceLength = 0;
        for (;;) {
            entry = &table->entries[index];
            if (entry->key == NULL) {
                break;
            }

            if (probeSequenceLength > entry->probeSequenceLength) {
                SWAP(Value, value, entry->value);
                SWAP(ObjString*, key, entry->key);
                SWAP(int, probeSequenceLength, entry->probeSequenceLength);
            }

            index = (index + 1) % table->capacity;
            probeSequenceLength++;
        }

        if (IS_NIL(entry->value)) table->count++; // If it's not a tombstone, update count
        entry->key = key;
        entry->value = value;
        entry->probeSequenceLength = probeSequenceLength;
        return true;
    }

    entry->key = key;
    entry->value = value;
    return false;
}

bool tableDelete(Table* table, ObjString* key) {
    if (table->count == 0) return false;

    // Find the entry.
    Entry* entry = findEntry(table->entries, table->capacity, key, false);
    if (entry == NULL || entry->key == NULL) return false;

    // Place a tombstone in the entry.
    entry->key = NULL;
    entry->value = BOOL_VAL(true);
    return true;
}

void tableAddAll(Table* from, Table* to) {
    for (int i = 0; i < from->capacity; i++) {
        Entry* entry = &from->entries[i];
        if (entry->key != NULL) {
            tableSet(to, entry->key, entry->value);
        }
    }
}

ObjString* tableFindString(Table* table, const char* chars, int length, uint32_t hash) {
    if (table->count == 0) return NULL;

    uint32_t index = hash % table->capacity;
    for (;;) {
        Entry* entry = &table->entries[index];
        if (entry->key == NULL) {
            // Stop if we find an empty non-tombstone key.
            if (IS_NIL(entry->value)) return NULL;
        } else if (entry->key->length == length
                && entry->key->hash == hash
                && memcmp(entry->key->chars, chars, length) == 0) {
            // We found it.
            return entry->key;
        }

        index = (index + 1) % table->capacity;
    }
}

void tableRemoveWhite(Table* table) {
    for (int i = 0; i < table->capacity; i++) {
        Entry* entry = &table->entries[i];
        if (entry->key != NULL && !entry->key->obj.isMarked) {
            tableDelete(table, entry->key);
        }
    }
}

void markTable(Table* table) {
    for (int i = 0; i < table->capacity; i++) {
        Entry* entry = &table->entries[i];
        markObject((Obj*)entry->key);
        markValue(entry->value);
    }
}

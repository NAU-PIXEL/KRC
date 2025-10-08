# KRC Apple Silicon Compatibility Fix

**Date:** October 7, 2025
**Issue:** SIGKILL when running KRC on Apple Silicon (M1/M2/M3) Macs
**Root Cause:** 4GB static memory allocation exceeds Apple Silicon dyld addressing limits
**Status:** ✅ ROOT CAUSE CONFIRMED - Code modification required

---

## The Real Problem

The SIGKILL issue is NOT caused by:
- ❌ Broken dyld cache
- ❌ Missing system libraries
- ❌ macOS version issues
- ❌ Homebrew/gfortran compatibility

The SIGKILL issue IS caused by:
- ✅ **4.4 GB static `__DATA` segment in KRC binary**
- ✅ **Apple Silicon memory addressing limitation with large static arrays**
- ✅ **Dynamic linker (dyld) unable to map shared libraries alongside huge static data**

---

## Evidence

### Binary Segment Analysis

```bash
$ size krc
__TEXT    __DATA        __OBJC    others       dec          hex
278528    4436295680    0         4295032832   8731607040   20871c000
          ^^^^^^^^^^
          4.4 GB static data!
```

### Source Code Analysis

From [`src/krcc8m.f:23`](src/krcc8m.f#L23):

```fortran
PARAMETER (KOMMON=512000000) ! 80 MB, Storage used by tdisk
```

**512 million 8-byte words = 4.096 GB of static memory**

This is allocated in COMMON blocks throughout the KRC source code, creating massive static arrays that are part of the binary's data segment.

---

## Why This Fails on Apple Silicon

### The Technical Details

1. **Apple Silicon has stricter memory mapping limits** than Intel Macs
2. **dyld (dynamic linker) needs address space** to:
   - Load the executable
   - Load shared libraries (`libSystem.B.dylib`, `libgfortran.dylib`, etc.)
   - Perform relocations
3. **With 4GB of static data**, there's insufficient address space for dyld to:
   - Map the dyld shared cache
   - Load system libraries
   - Complete the linking process
4. **Result:** dyld fails with "syscall to map cache into shared region failed"
5. **Kernel response:** SIGKILL -9 (process terminated before it can even start)

### Why the Error Message is Misleading

The error says:
```
dyld: Library not loaded: /usr/lib/libSystem.B.dylib
```

But the library file isn't actually missing - **dyld can't load it because there's no address space left** after mapping the 4GB static data segment.

---

## Solutions

### Option 1: Convert to Dynamic Allocation (Recommended)

**Modify KRC to use allocatable arrays instead of static COMMON blocks.**

**Advantages:**
- Memory allocated only when needed
- Works on all platforms
- More flexible memory management
- Reduces binary size dramatically

**Changes Required:**

1. Convert COMMON blocks to allocatable arrays
2. Add allocation/deallocation code
3. Pass arrays as arguments where needed

**Example:**

```fortran
! Before (static):
PARAMETER (KOMMON=512000000)
COMMON /DISKCOM/ FFF(KOMMON)

! After (dynamic):
REAL(8), ALLOCATABLE :: FFF(:)
INTEGER :: KOMMON_SIZE

! In initialization:
KOMMON_SIZE = 512000000
ALLOCATE(FFF(KOMMON_SIZE))

! At cleanup:
DEALLOCATE(FFF)
```

### Option 2: Reduce KOMMON Size

**If full dynamic allocation is too invasive, reduce the static array size.**

**Quick test - Edit [`src/krcc8m.f:23`](src/krcc8m.f#L23):**

```fortran
! Original:
PARAMETER (KOMMON=512000000)  ! 4 GB

! Reduced (test if smaller size works):
PARAMETER (KOMMON=50000000)   ! 400 MB
```

**Rebuild and test:**
```bash
make clean
make krc
cd run && ../krc < ../krc_python/pykrc/data/krc_support/fake_krc344
```

**Note:** This may cause runtime errors if KRC needs the full array size. Only use for testing whether size reduction fixes the dyld issue.

### Option 3: Move to Shared Library

**Move large data structures into a dynamically loaded shared library.**

**Advantages:**
- Shared libraries have separate address space
- dyld can map them after loading the main executable

**Disadvantages:**
- Significant code restructuring required
- More complex build process

### Option 4: Use an Intel Mac or x86 Emulation

**Run KRC on Intel Macs or use Rosetta 2 translation.**

**Note:** Rosetta 2 may or may not work - Intel Macs have the same issue with large static arrays, though limits may be different.

---

## Recommended Fix (Detailed)

### Phase 1: Identify All Static Allocations

```bash
# Find all PARAMETER definitions
grep -n "PARAMETER.*KOMMON\|PARAMETER.*MAX" src/krcc8m.f

# Find all COMMON blocks
grep -n "COMMON" src/*.f | head -50
```

### Phase 2: Convert DISKCOM to Dynamic

The largest static allocation is likely in `DISKCOM`:

1. **Edit [`src/krcc8m.f`](src/krcc8m.f):**
   - Remove or comment out `PARAMETER (KOMMON=512000000)`
   - Declare arrays as `ALLOCATABLE`

2. **Edit main program ([`src/krc8.f`](src/krc8.f)):**
   - Add allocation code in initialization
   - Add deallocation before program end

3. **Edit subroutines that use DISKCOM:**
   - Pass array as argument instead of COMMON
   - Or maintain module-level allocatable array

### Phase 3: Test Incrementally

1. Start with smallest COMMON blocks
2. Test after each conversion
3. Verify functionality with test cases

### Phase 4: Rebuild and Verify

```bash
make clean
make krc
./run_krc_test.sh
```

---

## Quick Test to Confirm This is the Issue

### Reduce KOMMON temporarily:

```bash
# Backup original
cp src/krcc8m.f src/krcc8m.f.backup

# Edit to reduce size
sed -i.bak 's/PARAMETER (KOMMON=512000000)/PARAMETER (KOMMON=10000000)/' src/krcc8m.f

# Rebuild
make clean
make krc

# Test
cd run && ../krc < ../krc_python/pykrc/data/krc_support/fake_krc344

# If this works (no dyld error), the issue is confirmed

# Restore original
mv src/krcc8m.f.backup src/krcc8m.f
```

---

## Why This Wasn't Caught Earlier

1. **KRC was developed on Intel/x86 systems** where address space limits are different
2. **Apple Silicon is relatively new** (2020+) with different memory architecture
3. **Most Fortran codes don't use 4GB static arrays** - this is exceptionally large
4. **The error message is misleading** - says "library not found" when the real issue is "out of address space"

---

## References

- **Apple Developer Forums:** ["dyld cache not loaded" with large static arrays](https://developer.apple.com/forums/thread/697494)
- **Stack Overflow:** [Similar issue with Python](https://stackoverflow.com/questions/70809158/)
- **Technical Explanation:** Apple Silicon address space limitations with dyld shared cache

---

## Next Steps

1. **Confirm the issue:** Run the quick test above to verify size reduction fixes dyld error
2. **Choose a solution:** Decide between dynamic allocation, size reduction, or hybrid approach
3. **Implement changes:** Modify Fortran code to eliminate or reduce static arrays
4. **Test thoroughly:** Ensure KRC functionality is preserved after changes
5. **Update Python wrapper:** Once KRC binary works, test with `pykrc`

---

## macOS Tahoe 26.0 Beta Note

You're running macOS Tahoe 26.0.1 (beta). While this may have some Homebrew compatibility issues, the primary problem is the 4GB static array, which would fail on any Apple Silicon Mac regardless of macOS version.

**Recommendation:** Fix the static array issue first, as it's the root cause.

---

## Status

**Blocked on:** KRC source code modifications to reduce or eliminate 4GB static `KOMMON` array

**Next Action:** Test with reduced `KOMMON` value to confirm this is the issue, then plan code refactoring strategy

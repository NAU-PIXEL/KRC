#!/bin/bash
# Quick test script to run KRC through davinci and check for SIGKILL
# This helps determine if the issue is in the KRC binary or the Python wrapper

set -e  # Exit on error

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${YELLOW}KRC Davinci Test Runner${NC}"
echo "================================"
echo ""

# Check if KRC binary exists
if [ ! -f "/Users/chaberle/Documents/GitHab/KRC/krc" ]; then
    echo -e "${RED}ERROR: KRC binary not found!${NC}"
    echo "Building KRC binary..."
    cd /Users/chaberle/Documents/GitHab/KRC
    make clean && make krc
    if [ $? -ne 0 ]; then
        echo -e "${RED}Failed to build KRC binary${NC}"
        exit 1
    fi
    echo -e "${GREEN}KRC binary built successfully${NC}"
fi

# Set environment variables
export DV_KRC_HOME="/Users/chaberle/Documents/GitHab/KRC"
export DV_SCRIPT_FILES="/Users/chaberle/Documents/GitHab/KRC/krc_python/pykrc/data"

echo "Environment:"
echo "  DV_KRC_HOME=$DV_KRC_HOME"
echo "  DV_SCRIPT_FILES=$DV_SCRIPT_FILES"
echo ""

# Test 1: Run KRC binary directly with a simple input
echo -e "${YELLOW}Test 1: Direct KRC binary test${NC}"
echo "Running KRC binary directly with fake input..."
cd /Users/chaberle/Documents/GitHab/KRC/run
if timeout 10 /Users/chaberle/Documents/GitHab/KRC/krc < /Users/chaberle/Documents/GitHab/KRC/krc_python/pykrc/data/krc_support/fake_krc344 > /dev/null 2>&1; then
    echo -e "${GREEN}✓ KRC binary runs without SIGKILL${NC}"
else
    EXIT_CODE=$?
    if [ $EXIT_CODE -eq 137 ]; then
        echo -e "${RED}✗ KRC binary received SIGKILL (exit code 137)${NC}"
        echo "  This indicates the problem is in the KRC binary itself, not the Python wrapper"
    elif [ $EXIT_CODE -eq 124 ]; then
        echo -e "${YELLOW}⚠ KRC binary timed out after 10 seconds${NC}"
    else
        echo -e "${RED}✗ KRC binary failed with exit code $EXIT_CODE${NC}"
    fi
fi
echo ""

# Test 2: Run davinci test
echo -e "${YELLOW}Test 2: Davinci interface test${NC}"
echo "Running simple KRC test through davinci..."
cd /Users/chaberle/Documents/GitHab/KRC

# Create a minimal test script
cat > /tmp/test_krc_minimal.dv <<'EOF'
# Minimal KRC test
result = krc(lat=12.)
if (HasValue(result)) {
    printf("SUCCESS: KRC completed\n")
    printf("Temperature range: %.2f - %.2f K\n", min(result.tsurf), max(result.tsurf))
    exit(0)
} else {
    printf("FAILED: No result returned\n")
    exit(1)
}
EOF

if timeout 30 /Applications/davinci.app/Contents/Resources/bin/davinci \
    -f /Users/chaberle/Documents/GitHab/KRC/krc_python/docs/davinci/krc.dvrc \
    -f /tmp/test_krc_minimal.dv 2>&1 | tee /tmp/davinci_test.log; then

    if grep -q "SUCCESS" /tmp/davinci_test.log; then
        echo -e "${GREEN}✓ Davinci KRC interface works${NC}"
    else
        echo -e "${YELLOW}⚠ Davinci completed but check output${NC}"
    fi
else
    EXIT_CODE=$?
    if [ $EXIT_CODE -eq 137 ]; then
        echo -e "${RED}✗ Davinci test received SIGKILL${NC}"
    elif [ $EXIT_CODE -eq 124 ]; then
        echo -e "${YELLOW}⚠ Davinci test timed out${NC}"
    else
        echo -e "${RED}✗ Davinci test failed with exit code $EXIT_CODE${NC}"
    fi
fi

echo ""
echo "================================"
echo -e "${YELLOW}Summary${NC}"
echo "If both tests pass: Issue is likely in the Python wrapper"
echo "If both fail with SIGKILL: Issue is in KRC binary or system limits"
echo "If only davinci fails: Issue is in how davinci invokes KRC"
echo ""
echo "For interactive testing, run:"
echo "  ./setup_davinci_krc.sh"

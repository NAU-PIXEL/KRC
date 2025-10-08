#!/bin/bash
# Setup script for running davinci with KRC interface
# This script sets up the environment variables needed by the krc.dvrc file

# Set the KRC home directory (root of the KRC project)
export DV_KRC_HOME="/Users/chaberle/Documents/GitHab/KRC"

# Set the script files directory (where krc_support data files are located)
# The davinci interface expects these files in $DV_SCRIPT_FILES/krc_support/
export DV_SCRIPT_FILES="/Users/chaberle/Documents/GitHab/KRC/krc_python/pykrc/data"

# Verify KRC binary exists
if [ ! -f "$DV_KRC_HOME/krc" ]; then
    echo "ERROR: KRC binary not found at $DV_KRC_HOME/krc"
    echo "Please run 'make krc' in $DV_KRC_HOME to build it"
    exit 1
fi

# Verify krc_support directory exists
if [ ! -d "$DV_SCRIPT_FILES/krc_support" ]; then
    echo "ERROR: krc_support directory not found at $DV_SCRIPT_FILES/krc_support"
    exit 1
fi

echo "Environment variables set:"
echo "  DV_KRC_HOME=$DV_KRC_HOME"
echo "  DV_SCRIPT_FILES=$DV_SCRIPT_FILES"
echo ""
echo "KRC binary: $DV_KRC_HOME/krc"
echo "Support files: $DV_SCRIPT_FILES/krc_support/"
echo ""
echo "Starting davinci with KRC interface..."
echo ""

# Start davinci and load the krc.dvrc file
exec /Applications/davinci.app/Contents/Resources/bin/davinci \
    -f "$DV_KRC_HOME/krc_python/docs/davinci/krc.dvrc"

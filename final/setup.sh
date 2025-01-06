#!/bin/bash

# setup.sh: Remove any prior haunt-as installation and reinstall it.

# Exit immediately if a command exits with a non-zero status
set -e

# Function to display messages
echo_info() {
    echo -e "\033[1;34m[INFO]\033[0m $1"
}

echo_error() {
    echo -e "\033[1;31m[ERROR]\033[0m $1" >&2
}

# Define Racket's user-level collects directory
USER_COLLECTS_DIR="$HOME/racket/collects"

# Define the haunt-as collection path
HAUNT_AS_COLLECTION="$USER_COLLECTS_DIR/haunt-as"

# Define the source directory of haunt-as (assuming setup.sh is in project root)
SOURCE_HAUNT_AS_DIR="$(pwd)/haunt-as"

# Check if the source directory exists
if [ ! -d "$SOURCE_HAUNT_AS_DIR" ]; then
    echo_error "Source haunt-as directory not found at $SOURCE_HAUNT_AS_DIR."
    echo_error "Please ensure that the 'haunt-as' directory exists in the project root."
    exit 1
fi

# Function to remove existing haunt-as collection
remove_existing_haunt_as() {
    if [ -d "$HAUNT_AS_COLLECTION" ]; then
        echo_info "Removing existing haunt-as collection at $HAUNT_AS_COLLECTION..."
        rm -rf "$HAUNT_AS_COLLECTION"
        echo_info "Successfully removed existing haunt-as."
    else
        echo_info "No existing haunt-as collection found at $HAUNT_AS_COLLECTION."
    fi
}

# Function to install new haunt-as collection
install_haunt_as() {
    echo_info "Installing new haunt-as collection..."

    # Create necessary directories
    mkdir -p "$HAUNT_AS_COLLECTION/lang"

    # Copy main language module as main.rkt
    cp "$SOURCE_HAUNT_AS_DIR/haunt-as.rkt" "$HAUNT_AS_COLLECTION/main.rkt"
    echo_info "Copied haunt-as.rkt to $HAUNT_AS_COLLECTION/main.rkt"

    # Copy reader and expander to lang/
    cp "$SOURCE_HAUNT_AS_DIR/reader.rkt" "$HAUNT_AS_COLLECTION/lang/"
    echo_info "Copied reader.rkt to $HAUNT_AS_COLLECTION/lang/"

    cp "$SOURCE_HAUNT_AS_DIR/expander.rkt" "$HAUNT_AS_COLLECTION/lang/"
    echo_info "Copied expander.rkt to $HAUNT_AS_COLLECTION/lang/"

    # Copy input.txt to the collection directory
#    cp "$SOURCE_HAUNT_AS_DIR/input.txt" "$HAUNT_AS_COLLECTION/"
#    echo_info "Copied input.txt to $HAUNT_AS_COLLECTION/"

    echo_info "Successfully installed haunt-as collection at $HAUNT_AS_COLLECTION."
}

# Function to ensure the user collects directory exists
ensure_user_collects_dir() {
    if [ ! -d "$USER_COLLECTS_DIR" ]; then
        echo_info "User collects directory $USER_COLLECTS_DIR does not exist. Creating it..."
        mkdir -p "$USER_COLLECTS_DIR"
        echo_info "Successfully created $USER_COLLECTS_DIR."
    else
        echo_info "User collects directory $USER_COLLECTS_DIR already exists."
    fi
}

# Ensure user collects directory exists
ensure_user_collects_dir

# Remove existing haunt-as collection if any
remove_existing_haunt_as

# Install new haunt-as collection
install_haunt_as

echo_info "haunt-as setup completed successfully."


-- Database Schema for Secure Messaging App

CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

-- Users Table
CREATE TABLE users (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    email VARCHAR(255) UNIQUE NOT NULL,
    password_hash VARCHAR(255) NOT NULL,
    username VARCHAR(100) NOT NULL,
    public_key TEXT NOT NULL,
    profile_picture_url TEXT,
    last_seen TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Messages Table
CREATE TABLE messages (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    sender_id UUID REFERENCES users(id) ON DELETE CASCADE,
    receiver_id UUID REFERENCES users(id) ON DELETE CASCADE,
    encrypted_message TEXT, -- The AES encrypted message
    encrypted_key TEXT,    -- The AES key encrypted with receiver's public key
    encrypted_file_url TEXT,
    status VARCHAR(20) DEFAULT 'sending', -- sending, sent, delivered, read
    is_media BOOLEAN DEFAULT FALSE,
    timestamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Contacts Table (Optional, can be inferred from messages but useful for faster lookups)
CREATE TABLE contacts (
    user_id UUID REFERENCES users(id),
    contact_id UUID REFERENCES users(id),
    PRIMARY KEY (user_id, contact_id)
);

CREATE INDEX idx_messages_receiver ON messages(receiver_id);
CREATE INDEX idx_messages_sender ON messages(sender_id);

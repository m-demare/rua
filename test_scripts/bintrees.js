// Revised BSD license

// This is a specific instance of the Open Source Initiative (OSI) BSD license template.

// Copyright © 2004-2008 Brent Fulgham, 2005-2023 Isaac Gouy

// All rights reserved.

// Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

//     Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

//     Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

//     Neither the name "The Computer Language Benchmarks Game" nor the name "The Benchmarks Game" nor the name "The Computer Language Shootout Benchmarks" nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

function main() {
    const maxDepth = Math.max(6, +process.argv[2] || 10);
    const stretchDepth = maxDepth + 1;
    const stretchTree = createTree(stretchDepth)
    console.log(`stretch tree of depth ${stretchDepth}\t check: ${checksum(stretchTree)}`)
    const longLivedTree = createTree(maxDepth);

    for (let depth = 4; depth <= maxDepth; depth += 2) {
        const iterations = 1 << maxDepth - depth + 4;
        let sum = 0;
        for (var i = 0; i < iterations; i++) {
            const tree = createTree(depth)
            sum += checksum(tree)
        }
        console.log(`${iterations}\t trees of depth ${depth}\t check: ${sum}`)
    }

    console.log(
        `long lived tree of depth ${maxDepth}\t check: ${checksum(longLivedTree)}`
    );
}

function checksum(node) {
    if (!node.left) {
        return 1;
    }
    return 1 + checksum(node.left) + checksum(node.right);
}

function createTree(depth) {
    return depth-- > 0
        ? { left: createTree(depth), right: createTree(depth) }
        : { left: null, right: null };
}

main()

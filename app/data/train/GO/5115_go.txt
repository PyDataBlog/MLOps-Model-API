// Copyright 2014 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package size

import (
	"math"
	"time"
)

// Size returns the number of bytes that can be transferred given a
// bytesPerSecond rate over dur time.
func Size(bytesPerSecond int64, dur time.Duration) (numBytes int64) {
	return bytesPerSecond * int64(dur/time.Second)
}

// Rate returns the byte-rate needed to transfer numBytes in dur time.
func Rate(numBytes int64, dur time.Duration) (bytesPerSecond int64) {
	return numBytes / int64(dur/time.Second)
}

// Duration returns the time interval needed to complete a transfer of
// numBytes size at a bytesPerSecond rate.
func Duration(numBytes, bytesPerSecond int64) time.Duration {
	r := time.Duration(bytesPerSecond)
	s := time.Duration(numBytes)
	if s > math.MaxInt64/time.Second {
		// avoid overflow extreme case by using less-accurate calculation
		return s / r * time.Second
	}
	return s * time.Second / r
}

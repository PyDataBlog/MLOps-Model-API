<?php
/**
 * @package axy\fs\ifs
 * @author Oleg Grigoriev <go.vasac@gmail.com>
 */

namespace axy\fs\ifs;

/**
 * The file stream meta data
 *
 * @SuppressWarnings(PHPMD.CamelCasePropertyName)
 */
class MetaData
{
    /**
     * The file name
     *
     * @var string
     */
    public $filename;

    /**
     * TRUE if the stream timed out while waiting for data on the last call
     *
     * @var bool
     */
    public $timed_out;

    /**
     * TRUE if the stream is in blocking IO mode
     *
     * @var bool
     */
    public $blocked;

    /**
     * TRUE if the stream has reached end-of-file
     *
     * @var bool
     */
    public $eof;

    /**
     * The number of bytes currently contained in the PHP's own internal buffer
     *
     * @var int
     */
    public $unread_bytes;

    /**
     * A label describing the underlying implementation of the stream
     *
     * @var string
     */
    public $stream_type;

    /**
     * The type of access required for this file
     *
     * @var string
     */
    public $mode;

    /**
     * Whether the current stream can be seeked
     *
     * @var bool
     */
    public $seekable;

    /**
     * The file name
     *
     * @var string
     */
    public $uri;

    /**
     * The constructor
     *
     * @param array $data [optional]
     */
    public function __construct(array $data = null)
    {
        if ($data !== null) {
            foreach ($this as $k => &$v) {
                if (isset($data[$k])) {
                    $v = $data[$k];
                }
            }
            unset($v);
        }
        $this->filename = $this->uri;
    }
}
